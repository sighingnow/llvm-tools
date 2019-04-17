#include <iostream>
#include <memory>
#include <set>

#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/Decl.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Basic/Diagnostic.h>
#include <clang/CodeGen/BackendUtil.h>
#include <clang/CodeGen/CodeGenAction.h>
#include <clang/Driver/Compilation.h>
#include <clang/Driver/Driver.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendAction.h>
#include <clang/Frontend/FrontendOptions.h>
#include <clang/Frontend/Utils.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <llvm/Support/CommandLine.h>
#include <llvm/Support/TargetSelect.h>

#include <fmt/format.h>
#include <readline/readline.h>

class CShellASTConsumer : public clang::ASTConsumer {
   public:
    explicit CShellASTConsumer(clang::ASTContext &Context) {
    }

    virtual void HandleTranslationUnit(clang::ASTContext &Context) override {
        std::cout << "Handle Translation Unit ..." << std::endl;
    }
};

class CShellAction : public clang::CodeGenAction {
   public:
    CShellAction(clang::BackendAction Act = clang::Backend_EmitLL,
                 llvm::LLVMContext *Context = nullptr)
            : clang::CodeGenAction(Act, Context) {
    }
    // FIXME
    // virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(clang::CompilerInstance &CI,
    //                                                               llvm::StringRef InFile) override {
    //     return std::unique_ptr<clang::ASTConsumer>(new CShellASTConsumer(CI.getASTContext()));
    // }
};

int repl(int argc, char const **argv) {
    // Create file system
    llvm::IntrusiveRefCntPtr<llvm::vfs::OverlayFileSystem> OverlayFS(
            new llvm::vfs::OverlayFileSystem(llvm::vfs::getRealFileSystem()));
    llvm::IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> InMemoryFS(
            new llvm::vfs::InMemoryFileSystem());
    OverlayFS->pushOverlay(InMemoryFS);

    llvm::IntrusiveRefCntPtr<clang::FileManager> FileManager(
            new clang::FileManager(clang::FileSystemOptions(), OverlayFS));

    // Create compiler and action
    auto PCH = std::make_shared<clang::PCHContainerOperations>();
    auto Diagnostics = clang::CompilerInstance::createDiagnostics(
            new clang::DiagnosticOptions(), new clang::DiagnosticConsumer(), true);
    clang::driver::Driver TheDriver("cshell", llvm::sys::getDefaultTargetTriple(), *Diagnostics,
                                    OverlayFS);
    TheDriver.setTitle("cshell");
    TheDriver.setCheckInputsExist(false);

    llvm::SmallVector<char const *, 16> Args(argv + 1, argv + argc);
    Args.push_back("-fsyntax-only");
    std::unique_ptr<clang::driver::Compilation> C(TheDriver.BuildCompilation(Args));
    fmt::print("compilation: {}\n", fmt::ptr(C.get()));

    auto CI = std::make_shared<clang::CompilerInvocation>();
    clang::CompilerInvocation::CreateFromArgs(*CI, const_cast<char const **>(Args.data()),
                                              const_cast<char const **>(Args.data() + Args.size()),
                                              *Diagnostics);
    fmt::print("compiler invocation: {}\n", fmt::ptr(CI.get()));

    auto Clang = std::make_unique<clang::CompilerInstance>(PCH);
    Clang->setInvocation(std::move(CI));
    fmt::print("has invocation: {}\n", Clang->hasInvocation());

    // NB: here the `false` is important, since the client is owned by previously created
    // DiagnosticsEngine.
    Clang->createDiagnostics(Diagnostics->getClient(), false);

    Clang->setTarget(clang::TargetInfo::CreateTargetInfo(Clang->getDiagnostics(),
                                                         Clang->getInvocation().TargetOpts));

#if LLVM_VERSION_MAJOR >= 10
    Clang->createFileManager(OverlayFS);
#else
    Clang->setVirtualFileSystem(OverlayFS);
    Clang->createFileManager();
#endif

    // Prepare LLVM JIT
    auto TSContext
            = std::make_shared<llvm::orc::ThreadSafeContext>(std::make_unique<llvm::LLVMContext>());

    auto ReplAction
            = std::make_shared<CShellAction>(clang::Backend_EmitLL, TSContext->getContext());
    ReplAction->PrepareToExecute(*Clang);

    // Prepare for repl
    std::string prompt = "> ";
    int round = 0;

    while (true) {
        char *buffer = readline(prompt.c_str());
        std::string Code(buffer);
        free(buffer);
        if (Code.compare(0, 2, ":q") == 0 || Code.compare(0, 4, "exit") == 0
            || Code.compare(0, 4, "quit") == 0) {
            break;
        }
        if (std::all_of(Code.begin(), Code.end(),
                        [](char const &ch) { return std::isspace(ch); })) {
            continue;
        }

        std::string FileName = fmt::format("cshell_repl_{}.cpp", round++);

        auto membuffer = llvm::MemoryBuffer::getMemBuffer(Code, FileName);
        InMemoryFS->addFile(FileName, 0, std::move(membuffer));
        clang::FrontendInputFile InputFile(
                FileName, clang::InputKind(clang::InputKind::CXX, clang::InputKind::Source, false));
        ReplAction->BeginSourceFile(*Clang, InputFile);
        ReplAction->Execute();
        ReplAction->EndSourceFile();

        auto Module = ReplAction->takeModule();
        fmt::print("module: {}\n", fmt::ptr(Module.get()));

#if defined(LLVM_ENABLE_DUMP)
        if (Module) {
            Module->dump();
        }
#endif
        Clang->getDiagnostics().dump();
    }

    // Shutdown.
    llvm::llvm_shutdown();
    return 0;
}

int main(int argc, char const **argv) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    return repl(argc, argv);
}

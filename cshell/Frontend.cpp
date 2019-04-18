#include "Frontend.h"

Frontend::Frontend(llvm::LLVMContext *Context)
        : OverlayFS(new llvm::vfs::OverlayFileSystem(llvm::vfs::getRealFileSystem())),
          InMemoryFS(new llvm::vfs::InMemoryFileSystem()),
          FileManager(new clang::FileManager(clang::FileSystemOptions(), OverlayFS)),
          Clang(new clang::CompilerInstance(std::make_shared<clang::PCHContainerOperations>())) {
    OverlayFS->pushOverlay(InMemoryFS);

    auto DiagOpts = new clang::DiagnosticOptions();
    Diagnostics = clang::CompilerInstance::createDiagnostics(
            DiagOpts, new clang::TextDiagnosticPrinter(llvm::errs(), DiagOpts, false), true);

    // NB: no need to recreate: Clang->createDiagnostics(Diagnostics->getClient(), false);
    //
    // here the `false` is important, since the client is owned by previously created
    // DiagnosticsEngine.
    Clang->setDiagnostics(&*Diagnostics);

    llvm::opt::ArgStringList Args;

#if defined(DEBUG)
    Args.push_back("-v");
#endif

    auto CI = std::make_shared<clang::CompilerInvocation>();
    clang::CompilerInvocation::CreateFromArgs(*CI, const_cast<char const **>(Args.data()),
                                              const_cast<char const **>(Args.data() + Args.size()),
                                              *Diagnostics);
    Clang->setInvocation(std::move(CI));

    auto &LangOpts = Clang->getLangOpts();
    LangOpts.CPlusPlus = 1;
    LangOpts.CPlusPlus17 = 1;
    LangOpts.CXXExceptions = 1;
    LangOpts.RTTI = 1;

    auto &HeaderSearchOpts = Clang->getHeaderSearchOpts();
    HeaderSearchOpts.UseBuiltinIncludes = 1;
    HeaderSearchOpts.UseStandardSystemIncludes = 1;
    HeaderSearchOpts.UseStandardCXXIncludes = 1;
    HeaderSearchOpts.Verbose = 0;

    // FIXME
    HeaderSearchOpts.AddPath(".", clang::frontend::CSystem, false, false);
    HeaderSearchOpts.AddPath("/usr/local/include", clang::frontend::CXXSystem, false, false);
    HeaderSearchOpts.AddPath("/usr/bin/../lib/gcc/x86_64-linux-gnu/8/../../../../include/c++/8",
                             clang::frontend::CXXSystem, false, false);
    HeaderSearchOpts.AddPath(
            "/usr/bin/../lib/gcc/x86_64-linux-gnu/8/../../../../include/x86_64-linux-gnu/c++/8",
            clang::frontend::CXXSystem, false, false);
    HeaderSearchOpts.AddPath(
            "/usr/bin/../lib/gcc/x86_64-linux-gnu/8/../../../../include/c++/8/backward",
            clang::frontend::CXXSystem, false, false);
    HeaderSearchOpts.AddPath("/usr/include/clang/9.0.0/include", clang::frontend::System, false,
                             false);
    HeaderSearchOpts.AddPath("/usr/include/x86_64-linux-gnu", clang::frontend::System, false,
                             false);
    HeaderSearchOpts.AddPath("/usr/include", clang::frontend::System, false, false);

    Clang->setTarget(clang::TargetInfo::CreateTargetInfo(Clang->getDiagnostics(),
                                                         Clang->getInvocation().TargetOpts));

#if LLVM_VERSION_MAJOR >= 10
    Clang->createFileManager(OverlayFS);
#else
    Clang->setVirtualFileSystem(OverlayFS);
    Clang->createFileManager();
#endif

    ReplAction = std::make_shared<CShellAction>(clang::Backend_EmitNothing, Context);
    ReplAction->PrepareToExecute(*Clang);
}

std::unique_ptr<llvm::Module> Frontend::runCode(std::string Code, std::string FileName) {
    auto membuffer = llvm::MemoryBuffer::getMemBuffer(Code, FileName);
    InMemoryFS->addFile(FileName, 0, std::move(membuffer));
    clang::FrontendInputFile InputFile(
            FileName, clang::InputKind(clang::InputKind::CXX, clang::InputKind::Source, false));
    ReplAction->BeginSourceFile(*Clang, InputFile);
    ReplAction->Execute();
    ReplAction->EndSourceFile();

    Diagnostics->Reset();
    return ReplAction->takeModule();
}
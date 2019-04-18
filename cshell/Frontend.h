#ifndef __LLVM_CSHELL_FRONTEND_H__
#define __LLVM_CSHELL_FRONTEND_H__

#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/Basic/Diagnostic.h>
#include <clang/CodeGen/BackendUtil.h>
#include <clang/CodeGen/CodeGenAction.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendAction.h>
#include <clang/Frontend/FrontendOptions.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <clang/Frontend/Utils.h>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Error.h>

#include "Common.h"

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
    //                                                               llvm::StringRef InFile)
    //                                                               override {
    //     return std::unique_ptr<clang::ASTConsumer>(new CShellASTConsumer(CI.getASTContext()));
    // }
};

class Frontend {
   public:
    Frontend(llvm::LLVMContext *Context)
            : OverlayFS(new llvm::vfs::OverlayFileSystem(llvm::vfs::getRealFileSystem())),
              InMemoryFS(new llvm::vfs::InMemoryFileSystem()),
              FileManager(new clang::FileManager(clang::FileSystemOptions(), OverlayFS)),
              Clang(new clang::CompilerInstance(
                      std::make_shared<clang::PCHContainerOperations>())) {
        OverlayFS->pushOverlay(InMemoryFS);

        auto DiagOpts = new clang::DiagnosticOptions();
        Diagnostics = clang::CompilerInstance::createDiagnostics(
                DiagOpts, new clang::TextDiagnosticPrinter(llvm::errs(), DiagOpts, false), true);

        // NB: no need to recreate: Clang->createDiagnostics(Diagnostics->getClient(), false);
        //
        // here the `false` is important, since the client is owned by previously created
        // DiagnosticsEngine.
        Clang->setDiagnostics(&*Diagnostics);

        llvm::SmallVector<char const *, 16> Args;
        Args.push_back("-fsyntax-only");
        auto CI = std::make_shared<clang::CompilerInvocation>();
        clang::CompilerInvocation::CreateFromArgs(
                *CI, const_cast<char const **>(Args.data()),
                const_cast<char const **>(Args.data() + Args.size()), *Diagnostics);
        Clang->setInvocation(std::move(CI));

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

    std::unique_ptr<llvm::Module> runCode(std::string Code, std::string FileName);

   private:
    llvm::IntrusiveRefCntPtr<llvm::vfs::OverlayFileSystem> OverlayFS;
    llvm::IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> InMemoryFS;
    llvm::IntrusiveRefCntPtr<clang::FileManager> FileManager;

    llvm::IntrusiveRefCntPtr<clang::DiagnosticsEngine> Diagnostics;
    std::unique_ptr<clang::CompilerInstance> Clang;
    std::shared_ptr<CShellAction> ReplAction;
};

#endif
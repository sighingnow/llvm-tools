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
    Frontend(llvm::LLVMContext *Context);

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
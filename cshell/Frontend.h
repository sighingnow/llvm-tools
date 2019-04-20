#ifndef __LLVM_CSHELL_FRONTEND_H__
#define __LLVM_CSHELL_FRONTEND_H__

#include <clang/CodeGen/BackendUtil.h>
#include <clang/Frontend/CompilerInstance.h>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "Common.h"

class DefaultCodegenAction;
class JITCodegenAction;

class Frontend {
   public:
    Frontend(llvm::LLVMContext &Context);

    std::unique_ptr<llvm::Module> runCode(std::string Code, std::string FileName);
    std::unique_ptr<llvm::Module> runCodeDefault(std::string Code, std::string FileName);

   private:
    llvm::LLVMContext &Context;

    llvm::IntrusiveRefCntPtr<llvm::vfs::OverlayFileSystem> OverlayFS;
    llvm::IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> InMemoryFS;
    llvm::IntrusiveRefCntPtr<clang::FileManager> FileManager;

    llvm::IntrusiveRefCntPtr<clang::DiagnosticsEngine> Diagnostics;
    std::unique_ptr<clang::CompilerInstance> Clang;
    std::shared_ptr<JITCodegenAction> JITAction;
    std::shared_ptr<DefaultCodegenAction> DefaultAction;
};

#endif
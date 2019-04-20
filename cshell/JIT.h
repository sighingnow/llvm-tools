#ifndef __LLVM_CSHELL_JIT_H__
#define __LLVM_CSHELL_JIT_H__

#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/TargetSelect.h>

#include "Common.h"

namespace orc = llvm::orc;

class JIT {
   public:
    JIT(llvm::DataLayout &&Layout, orc::JITTargetMachineBuilder &&JTMB,
        std::unique_ptr<llvm::TargetMachine> Machine, orc::ThreadSafeContext &JITContext);

    static llvm::Expected<std::unique_ptr<JIT>> Create(orc::ThreadSafeContext &JITContext);

    llvm::DataLayout const &getDataLayout() {
        return this->Layout;
    }

    llvm::LLVMContext &getContext() {
        return *JITContext.getContext();
    }

    void addIRModule(std::unique_ptr<llvm::Module> Module);

    llvm::Expected<llvm::JITEvaluatedSymbol> lookup(llvm::StringRef Name);

    llvm::Expected<llvm::JITEvaluatedSymbol> lookup(orc::SymbolStringPtr Name);

    bool hasSymbol(llvm::StringRef Name);

   private:
    llvm::Expected<orc::ThreadSafeModule> optimizer(orc::ThreadSafeModule Module,
                                                    orc::MaterializationResponsibility const &);

   private:
    llvm::DataLayout Layout;
    std::unique_ptr<llvm::TargetMachine> Machine;
    orc::ExecutionSession ES;
    orc::JITDylib &MainLib;
    orc::RTDyldObjectLinkingLayer LinkLayer;
    orc::IRCompileLayer CompileLayer;
    orc::IRTransformLayer TransformLayer;
    // orc::CompileOnDemandLayer CODLayer;
    orc::MangleAndInterner Mangle;
    orc::ThreadSafeContext &JITContext;
};

#endif
#ifndef __LLVM_CSHELL_JIT_H__
#define __LLVM_CSHELL_JIT_H__

#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

namespace orc = llvm::orc;

class JIT {
   public:
    JIT(llvm::DataLayout &&Layout, orc::JITTargetMachineBuilder &&JTMB,
        orc::ThreadSafeContext &JITContext)
            : Layout(std::move(Layout)),
              LinkLayer(ES, []() { return llvm::make_unique<llvm::SectionMemoryManager>(); }),
              CompileLayer(ES, LinkLayer, orc::ConcurrentIRCompiler(std::move(JTMB))),
              TransformLayer(ES, CompileLayer, optimizer),
              Mangle(ES, this->Layout),
              JITContext(JITContext) {
        ES.getMainJITDylib().setGenerator(llvm::cantFail(
                orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(this->Layout)));
    }

    static llvm::Expected<std::unique_ptr<JIT>> Create(orc::ThreadSafeContext &JITContext);

    llvm::DataLayout const &getDataLayout() {
        return this->Layout;
    }

    llvm::LLVMContext &getContext() {
        return *JITContext.getContext();
    }

    void addIRModule(std::unique_ptr<llvm::Module> Module);

    llvm::Expected<llvm::JITEvaluatedSymbol> lookup(llvm::StringRef Name);

   private:
    static llvm::Expected<orc::ThreadSafeModule> optimizer(
            orc::ThreadSafeModule Module, orc::MaterializationResponsibility const &);

   private:
    llvm::DataLayout Layout;
    orc::ExecutionSession ES;
    orc::RTDyldObjectLinkingLayer LinkLayer;
    orc::IRCompileLayer CompileLayer;
    orc::IRTransformLayer TransformLayer;
    // orc::CompileOnDemandLayer CODLayer;
    orc::MangleAndInterner Mangle;
    orc::ThreadSafeContext &JITContext;
};

#endif
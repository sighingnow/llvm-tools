#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include "JIT.h"

JIT::JIT(llvm::DataLayout&& Layout, orc::JITTargetMachineBuilder&& JTMB,
         std::unique_ptr<llvm::TargetMachine> Machine, orc::ThreadSafeContext& JITContext)
        : Layout(std::move(Layout)),
          Machine(std::move(Machine)),
          MainLib(ES.getMainJITDylib()),
          LinkLayer(ES, []() { return llvm::make_unique<llvm::SectionMemoryManager>(); }),
          CompileLayer(ES, LinkLayer, orc::ConcurrentIRCompiler(std::move(JTMB))),
          TransformLayer(ES, CompileLayer, std::bind(&JIT::optimizer, this, _1, _2)),
          Mangle(ES, this->Layout),
          JITContext(JITContext) {
    MainLib.setGenerator(
            llvm::cantFail(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(this->Layout)));
}

llvm::Expected<std::unique_ptr<JIT>> JIT::Create(orc::ThreadSafeContext& JITContext) {
    auto JTMB = orc::JITTargetMachineBuilder::detectHost();
    if (!JTMB) {
        return JTMB.takeError();
    }
    auto DL = JTMB->getDefaultDataLayoutForTarget();
    if (!DL) {
        return DL.takeError();
    }
    auto Machine = JTMB->createTargetMachine();
    if (!Machine) {
        return Machine.takeError();
    }
    return llvm::make_unique<JIT>(std::move(*DL), std::move(*JTMB), std::move(*Machine),
                                  JITContext);
}

void JIT::addIRModule(std::unique_ptr<llvm::Module> Module) {
    // Remove exists names.
    orc::SymbolNameSet names;
    for (auto&& GV : Module->globals()) {
        if (!GV.isDeclaration() && GV.hasName() && this->hasSymbol(GV.getName())) {
            names.insert(Mangle(GV.getName()));
        }
    }
    for (auto&& F : Module->functions()) {
        if (!F.isDeclaration() && F.hasName() && this->hasSymbol(F.getName())) {
            names.insert(Mangle(F.getName()));
        }
    }
    auto err = MainLib.remove(names);
    discardError(std::move(err));

    // Add IRModule
    Module->setDataLayout(this->Layout);
    err = TransformLayer.add(MainLib, orc::ThreadSafeModule(std::move(Module), JITContext));
    discardError(std::move(err));
}

llvm::Expected<llvm::JITEvaluatedSymbol> JIT::lookup(llvm::StringRef Name) {
    return ES.lookup(orc::JITDylibSearchList({{&ES.getMainJITDylib(), true}}), Mangle(Name));
}

llvm::Expected<llvm::JITEvaluatedSymbol> JIT::lookup(orc::SymbolStringPtr Name) {
    return ES.lookup(orc::JITDylibSearchList({{&ES.getMainJITDylib(), true}}), Name);
}

bool JIT::hasSymbol(llvm::StringRef Name) {
    return discardError(lookup(Name), false);
}

llvm::Expected<orc::ThreadSafeModule> JIT::optimizer(orc::ThreadSafeModule Module,
                                                     orc::MaterializationResponsibility const&) {
    auto FPM = std::make_unique<llvm::legacy::PassManager>();
    FPM->add(llvm::createCFGSimplificationPass());
    FPM->add(llvm::createCorrelatedValuePropagationPass());
    FPM->add(llvm::createDeadStoreEliminationPass());
    FPM->add(llvm::createGVNPass());
    FPM->add(llvm::createInstructionCombiningPass());
    FPM->add(llvm::createMemCpyOptPass());
    FPM->add(llvm::createMergedLoadStoreMotionPass());
    FPM->add(llvm::createReassociatePass());
    FPM->add(llvm::createSCCPPass());
    FPM->add(llvm::createSimpleLoopUnrollPass());
    FPM->add(llvm::createSimpleLoopUnrollPass());
    FPM->add(llvm::createSpeculativeExecutionIfHasBranchDivergencePass());
    FPM->add(llvm::createTailCallEliminationPass());

    // #if defined(LLVM_ENABLE_DUMP)
    //     Machine->addPassesToEmitFile(*FPM, static_cast<llvm::raw_fd_ostream&>(llvm::errs()),
    //     nullptr,
    //                                  llvm::TargetMachine::CGFT_AssemblyFile);
    // #endif

    FPM->run(*Module.getModule());

#if defined(LLVM_ENABLE_DUMP)
    for (auto&& F : Module.getModule()->functions()) {
        F.dump();
    }
#endif

    return std::forward<orc::ThreadSafeModule>(Module);
}

#include "JIT.h"

llvm::Expected<std::unique_ptr<JIT>> JIT::Create(orc::ThreadSafeContext& JITContext) {
    auto JTMB = orc::JITTargetMachineBuilder::detectHost();
    if (!JTMB) {
        return JTMB.takeError();
    }
    auto DL = JTMB->getDefaultDataLayoutForTarget();
    if (!DL) {
        return DL.takeError();
    }
    return llvm::make_unique<JIT>(std::move(*DL), std::move(*JTMB), JITContext);
}

void JIT::addIRModule(std::unique_ptr<llvm::Module> Module) {
    Module->setDataLayout(this->Layout);
    auto err = TransformLayer.add(ES.getMainJITDylib(),
                                      orc::ThreadSafeModule(std::move(Module), JITContext));
    discardError(std::move(err));
}

llvm::Expected<llvm::JITEvaluatedSymbol> JIT::lookup(llvm::StringRef Name) {
    return ES.lookup(orc::JITDylibSearchList({{&ES.getMainJITDylib(), true}}), Mangle(Name));
}

llvm::Expected<orc::ThreadSafeModule> JIT::optimizer(orc::ThreadSafeModule Module,
                                                     orc::MaterializationResponsibility const&) {
    auto FPM = std::make_unique<llvm::legacy::FunctionPassManager>(Module.getModule());
    FPM->add(llvm::createInstructionCombiningPass());
    FPM->add(llvm::createReassociatePass());
    FPM->add(llvm::createGVNPass());
    FPM->add(llvm::createCFGSimplificationPass());
    FPM->doInitialization();

    for (auto&& F : *Module.getModule()) {
        FPM->run(F);
    }
#if defined(LLVM_ENABLE_DUMP)
    Module.getModule()->dump();
#endif
    return std::forward<orc::ThreadSafeModule>(Module);
}

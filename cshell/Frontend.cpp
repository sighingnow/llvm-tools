#include "Frontend.h"

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
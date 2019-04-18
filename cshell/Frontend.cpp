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

    llvm::SmallVector<char const *, 16> Args;
    auto CI = std::make_shared<clang::CompilerInvocation>();
    clang::CompilerInvocation::CreateFromArgs(*CI, const_cast<char const **>(Args.data()),
                                              const_cast<char const **>(Args.data() + Args.size()),
                                              *Diagnostics);
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
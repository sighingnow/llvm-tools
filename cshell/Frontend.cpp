#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/Basic/Diagnostic.h>
#include <clang/CodeGen/CodeGenAction.h>
#include <clang/CodeGen/ModuleBuilder.h>
#include <clang/Frontend/FrontendAction.h>
#include <clang/Frontend/FrontendOptions.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <clang/Lex/HeaderSearch.h>
#include <llvm/Option/Option.h>

#include "Frontend.h"

class JITASTCodegen : public clang::ASTConsumer {
   private:
    clang::CompilerInstance &Clang;
    clang::ASTContext *Context;
    std::unique_ptr<clang::CodeGenerator> Gen;

   public:
    explicit JITASTCodegen(clang::CompilerInstance &Clang, llvm::StringRef ModuleName,
                           llvm::LLVMContext &Context, clang::CoverageSourceInfo *CovInfo = nullptr)
            : Clang(Clang),
              Context(nullptr),
              Gen(clang::CreateLLVMCodeGen(Clang.getDiagnostics(), ModuleName,
                                           Clang.getHeaderSearchOpts(), Clang.getPreprocessorOpts(),
                                           Clang.getCodeGenOpts(), Context, CovInfo)) {
    }

    void Initialize(clang::ASTContext &Ctx) override {
        Context = &Ctx;
        Gen->Initialize(Ctx);
    }

    bool HandleTopLevelDecl(clang::DeclGroupRef D) override {
        return Gen->HandleTopLevelDecl(D);
    }

    void HandleInlineFunctionDefinition(clang::FunctionDecl *D) override {
        Gen->HandleInlineFunctionDefinition(D);
    }

    void HandleInterestingDecl(clang::DeclGroupRef D) override {
        // forward as default
        HandleTopLevelDecl(D);
    }

    void HandleTranslationUnit(clang::ASTContext &Ctx) override {
        Gen->HandleTranslationUnit(Ctx);
    }

    void HandleTagDeclDefinition(clang::TagDecl *D) override {
        Gen->HandleTagDeclDefinition(D);
    }

    void HandleTagDeclRequiredDefinition(clang::TagDecl const *D) override {
        Gen->HandleTagDeclRequiredDefinition(D);
    }

    void HandleCXXImplicitFunctionInstantiation(clang::FunctionDecl *D) override {
        Gen->HandleCXXImplicitFunctionInstantiation(D);
    }

    void HandleTopLevelDeclInObjCContainer(clang::DeclGroupRef D) override {
        Gen->HandleTopLevelDeclInObjCContainer(D);
    }

    void HandleImplicitImportDecl(clang::ImportDecl *D) override {
        Gen->HandleImplicitImportDecl(D);
    }

    void CompleteTentativeDefinition(clang::VarDecl *D) override {
        Gen->CompleteTentativeDefinition(D);
    }

    void AssignInheritanceModel(clang::CXXRecordDecl *RD) override {
        Gen->AssignInheritanceModel(RD);
    }

    void HandleCXXStaticMemberVarInstantiation(clang::VarDecl *D) override {
        Gen->HandleCXXStaticMemberVarInstantiation(D);
    }

    void HandleVTable(clang::CXXRecordDecl *RD) override {
        Gen->HandleVTable(RD);
    }

    clang::ASTMutationListener *GetASTMutationListener() override {
        return Gen->GetASTMutationListener();
    }

    clang::ASTDeserializationListener *GetASTDeserializationListener() override {
        return Gen->GetASTDeserializationListener();
    }

    void PrintStats() override {
        Gen->PrintStats();
    }

    bool shouldSkipFunctionBody(clang::Decl *D) override {
        return Gen->shouldSkipFunctionBody(D);
    }

    std::unique_ptr<llvm::Module> takeModule() {
        return std::unique_ptr<llvm::Module>(Gen->ReleaseModule());
    }

    ~JITASTCodegen() {
    }
};

class DefaultCodegenAction : public clang::CodeGenAction {
   public:
    DefaultCodegenAction(llvm::LLVMContext &Context,
                         clang::BackendAction Act = clang::Backend_EmitLL)
            : clang::CodeGenAction(Act, &Context) {
    }
};

class JITCodegenAction : public clang::ASTFrontendAction {
   private:
    llvm::LLVMContext &Context;
    clang::BackendAction Act;
    JITASTCodegen *Codegen = nullptr;
    std::unique_ptr<llvm::Module> TheModule;

   public:
    JITCodegenAction(llvm::LLVMContext &Context, clang::BackendAction Act = clang::Backend_EmitLL)
            : clang::ASTFrontendAction(), Context(Context), Act(Act) {
    }

    std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(clang::CompilerInstance &CI,
                                                          llvm::StringRef InFile) override {
        this->Codegen = new JITASTCodegen(CI, InFile, Context);
        return std::unique_ptr<clang::ASTConsumer>(this->Codegen);
    }

    bool hasIRSupport() const override {
        return true;
    }

    void EndSourceFileAction() override {
        // The llvm::Module must be taken before EndSourceFile.
        this->TheModule = Codegen->takeModule();
    }

    std::unique_ptr<llvm::Module> takeModule() {
        return std::move(this->TheModule);
    }
};

Frontend::Frontend(llvm::LLVMContext &Context)
        : Context(Context),
          OverlayFS(new llvm::vfs::OverlayFileSystem(llvm::vfs::getRealFileSystem())),
          InMemoryFS(new llvm::vfs::InMemoryFileSystem()),
          FileManager(new clang::FileManager(clang::FileSystemOptions(), OverlayFS)),
          Clang(new clang::CompilerInstance(std::make_shared<clang::PCHContainerOperations>())) {
    OverlayFS->pushOverlay(InMemoryFS);

    auto DiagOpts = new clang::DiagnosticOptions();
    DiagOpts->ShowColors = 1;
    DiagOpts->setShowOverloads(clang::Ovl_Best);
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

    JITAction = std::make_shared<JITCodegenAction>(Context, clang::Backend_EmitNothing);
    JITAction->PrepareToExecute(*Clang);

    DefaultAction = std::make_shared<DefaultCodegenAction>(Context, clang::Backend_EmitNothing);
    DefaultAction->PrepareToExecute(*Clang);
}

std::unique_ptr<llvm::Module> Frontend::runCode(std::string Code, std::string FileName) {
    auto membuffer = llvm::MemoryBuffer::getMemBuffer(Code, FileName);
    InMemoryFS->addFile(FileName, 0, std::move(membuffer));
    clang::FrontendInputFile InputFile(
            FileName, clang::InputKind(clang::InputKind::CXX, clang::InputKind::Source, false));
    JITAction->BeginSourceFile(*Clang, InputFile);
    JITAction->Execute();
    JITAction->EndSourceFile();

    Diagnostics->Reset();
    return JITAction->takeModule();
}

std::unique_ptr<llvm::Module> Frontend::runCodeDefault(std::string Code, std::string FileName) {
    auto membuffer = llvm::MemoryBuffer::getMemBuffer(Code, FileName);
    InMemoryFS->addFile(FileName, 0, std::move(membuffer));
    clang::FrontendInputFile InputFile(
            FileName, clang::InputKind(clang::InputKind::CXX, clang::InputKind::Source, false));
    DefaultAction->BeginSourceFile(*Clang, InputFile);
    DefaultAction->Execute();
    DefaultAction->EndSourceFile();

    Diagnostics->Reset();
    return DefaultAction->takeModule();
}

#include <iostream>
#include <memory>
#include <set>

#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/Decl.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Basic/Diagnostic.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendAction.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>

#include <llvm/Support/CommandLine.h>

namespace clang {
using namespace clang;
using namespace clang::tooling;
}  // namespace clang

namespace llvm {
using namespace llvm;
using namespace llvm::cl;
}  // namespace llvm

using llvm::StringRef;

class HelloVisitor : public clang::RecursiveASTVisitor<HelloVisitor> {
   private:
    clang::ASTContext &Context;
    clang::DiagnosticsEngine &Diag;

   public:
    explicit HelloVisitor(clang::ASTContext &Context)
            : Context(Context), Diag(Context.getDiagnostics()) {
    }

    explicit HelloVisitor(clang::CompilerInstance *CI)
            : Context(CI->getASTContext()), Diag(CI->getASTContext().getDiagnostics()) {
    }

    bool VisitFunctionDecl(clang::FunctionDecl *FD) {
        std::cout << "Function name: " << FD->getNameAsString() << std::endl;
        return true;
    }
};

class HelloASTConsumer : public clang::ASTConsumer {
   private:
    HelloVisitor Visitor;

   public:
    explicit HelloASTConsumer(clang::ASTContext &Context) : Visitor(Context) {
    }

    virtual void HandleTranslationUnit(clang::ASTContext &Context) override {
        Visitor.TraverseDecl(Context.getTranslationUnitDecl());
    }
};

class HelloFrontendAction : public clang::ASTFrontendAction {
   public:
    virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(clang::CompilerInstance &CI,
                                                                  StringRef InFile) override {
        return std::unique_ptr<clang::ASTConsumer>(new HelloASTConsumer(CI.getASTContext()));
    }
};

int runWithArgs(int argc, const char **argv) {
    auto clcat = llvm::OptionCategory("clang-parser", "clang-parser tool");
    clang::CommonOptionsParser opt(argc, argv, clcat);
    clang::ClangTool tool(opt.getCompilations(), opt.getSourcePathList());
    return tool.run(clang::newFrontendActionFactory<HelloFrontendAction>().get());
}

int main(int argc, const char **argv) {
    return clang::tooling::runToolOnCode(new HelloFrontendAction(), "int f() { return 0; }");
}

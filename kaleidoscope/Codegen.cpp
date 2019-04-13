#include <llvm/ADT/APFloat.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>

#include "kaleidoscope.h"

llvm::Value *scope_t::local(std::string const &name) {
    if (scopes.rbegin()->find(name) == scopes.rbegin()->end()) {
        return nullptr;
    } else {
        return scopes.rbegin()->at(name);
    }
}

llvm::Value *scope_t::global(std::string const &name) {
    for (auto iter = scopes.rbegin(); iter != scopes.rend(); iter++) {
        if (iter->find(name) != iter->end()) {
            return iter->at(name);
        }
    }
    return nullptr;
}

llvm::Value *number_t::codegen(llvm::Module *, llvm::LLVMContext &Context, llvm::IRBuilder<> &,
                               scope_t &) const {
    return llvm::ConstantFP::get(Context, llvm::APFloat(val));
}

llvm::Value *variable_t::codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                                 scope_t &Scope) const {
    if (auto r = Scope.global(name)) {
        return r;
    } else {
        throw std::runtime_error(fmt::format("Variable out of scope: {}", name));
    }
}

llvm::Value *unary_expr_t::codegen(llvm::Module *Module, llvm::LLVMContext &Context,
                                   llvm::IRBuilder<> &Builder, scope_t &Scope) const {
    llvm::Value *v = operand->codegen(Module, Context, Builder, Scope);
    switch (op) {
        case '!':
            return Builder.CreateNot(v);
        case '-':
            return Builder.CreateFNeg(v);
        default:
            throw std::runtime_error(fmt::format("Unknown unary operator: {}", op));
    }
}

llvm::Value *binary_expr_t::codegen(llvm::Module *Module, llvm::LLVMContext &Context,
                                    llvm::IRBuilder<> &Builder, scope_t &Scope) const {
    llvm::Value *l = lhs->codegen(Module, Context, Builder, Scope);
    llvm::Value *r = rhs->codegen(Module, Context, Builder, Scope);
    switch (op) {
        case '+':
            return Builder.CreateFAdd(l, r);
        case '-':
            return Builder.CreateFSub(l, r);
        case '*':
            return Builder.CreateFMul(l, r);
        case '/':
            return Builder.CreateFDiv(l, r);
        case '<':
            return Builder.CreateFCmpULT(l, r);
        case '>':
            return Builder.CreateFCmpUGT(l, r);
        case '=':
            return Builder.CreateFCmpUEQ(l, r);
        default:
            throw std::runtime_error(fmt::format("Unknown binary operator: {}", op));
    }
}

llvm::Value *call_t::codegen(llvm::Module *Module, llvm::LLVMContext &Context,
                             llvm::IRBuilder<> &Builder, scope_t &Scope) const {
    llvm::Function *F = Module->getFunction(callee);
    if (F == nullptr) {
        // The function from another module needs to be codegened again in this module.
        F = Scope.function(callee)->codegen(Module, Context, Builder, Scope);
    }
    std::vector<llvm::Value *> valargs(args.size());
    std::transform(args.begin(), args.end(), valargs.begin(),
                   [&](auto &&expr) { return expr->codegen(Module, Context, Builder, Scope); });
    return Builder.CreateCall(F, valargs);
}

llvm::Value *branch_t::codegen(llvm::Module *Module, llvm::LLVMContext &Context,
                               llvm::IRBuilder<> &Builder, scope_t &Scope) const {
    auto CondValue = condition->codegen(Module, Context, Builder, Scope);
    fmt::print("insert block: {:p}\n", fmt::ptr(Builder.GetInsertBlock()));
    auto F = Builder.GetInsertBlock()->getParent();

    // LLVM IR requires all basic blocks to be “terminated” with a control flow
    // instruction such as return or branch.

    auto If = llvm::BasicBlock::Create(Context, "then");
    auto Else = llvm::BasicBlock::Create(Context, "else");
    auto Merge = llvm::BasicBlock::Create(Context, "merge");
    Builder.CreateCondBr(CondValue, If, Else);

    F->getBasicBlockList().push_back(If);
    Builder.SetInsertPoint(If);
    auto IfValue = ifbody->codegen(Module, Context, Builder, Scope);
    Builder.CreateBr(Merge);
    If = Builder.GetInsertBlock();

    F->getBasicBlockList().push_back(Else);
    Builder.SetInsertPoint(Else);
    auto ElseValue = elsebody->codegen(Module, Context, Builder, Scope);
    Builder.CreateBr(Merge);
    Else = Builder.GetInsertBlock();

    F->getBasicBlockList().push_back(Merge);
    Builder.SetInsertPoint(Merge);
    llvm::PHINode *Phi = Builder.CreatePHI(llvm::Type::getDoubleTy(Context), 2, "phi");
    Phi->addIncoming(IfValue, If);
    Phi->addIncoming(ElseValue, Else);
    return Phi;
}

llvm::Function *prototype_t::codegen(llvm::Module *Module, llvm::LLVMContext &Context,
                                     llvm::IRBuilder<> &, scope_t &) const {
    std::vector<llvm::Type *> argtypes(args.size());
    for (size_t i = 0; i < args.size(); ++i) {
        argtypes[i] = llvm::Type::getDoubleTy(Context);
    }
    llvm::FunctionType *FType
            = llvm::FunctionType::get(llvm::Type::getDoubleTy(Context), argtypes, false);
    llvm::Function *F
            = llvm::Function::Create(FType, llvm::Function::ExternalLinkage, name, *Module);
    size_t idx = 0;
    for (auto &&Arg : F->args()) {
        Arg.setName(args[idx++]->getName());
    }
    return F;
}

llvm::Function *function_t::codegen(llvm::Module *Module, llvm::LLVMContext &Context,
                                    llvm::IRBuilder<> &Builder, scope_t &Scope) const {
    scope_t::RTTI rtti(Scope);
    llvm::Function *F = prototype->codegen(Module, Context, Builder, Scope);
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(Context, prototype->getName(), F);
    Builder.SetInsertPoint(BB);

    for (auto &&Arg : F->args()) {
        Scope.witness(Arg.getName(), &Arg);
    }
    if (llvm::Value *Ret = body->codegen(Module, Context, Builder, Scope)) {
        Builder.CreateRet(Ret);
        llvm::verifyFunction(*F);
        Scope.witnessFunction(prototype->getName(), prototype);
        return F;
    } else {
        // F->eraseFromParent();
        throw std::runtime_error(fmt::format("Failed codegen for function:\n{}", this->format()));
    }
}

std::vector<llvm::Function *> program_t::codegen(llvm::Module *Module, llvm::LLVMContext &Context,
                                                 llvm::IRBuilder<> &Builder, scope_t &Scope) const {
    std::vector<llvm::Function *> res;
    for (auto &&p : prototypes) {
        res.push_back(p->codegen(Module, Context, Builder, Scope));
    }
    for (auto &&f : functions) {
        res.push_back(f->codegen(Module, Context, Builder, Scope));
    }
    // wrap body into an entry function.
    auto entry = std::make_shared<function_t>("_main", std::vector<variable_ptr_t>{}, body);
    res.push_back(entry->codegen(Module, Context, Builder, Scope));

    // Module->dump();
    return res;
}

llvm::Function *procedure_t::codegen(llvm::Module *Module, llvm::LLVMContext &Context,
                                     llvm::IRBuilder<> &Builder, scope_t &Scope) const {
    return std::visit(
            overloaded{[&](prototype_ptr_t const &p) -> llvm::Function * {
                           return p->codegen(Module, Context, Builder, Scope);
                       },
                       [&](function_ptr_t const &f) -> llvm::Function * {
                           return f->codegen(Module, Context, Builder, Scope);
                       },
                       [&](expr_ptr_t const &e) -> llvm::Function * {
                           // wrap body into an entry function.
                           auto entry = std::make_shared<function_t>(
                                   fmt::format("{}_entry", std::string(Module->getName())),
                                   std::vector<variable_ptr_t>{}, e);
                           return entry->codegen(Module, Context, Builder, Scope);
                       }},
            this->prog);
}

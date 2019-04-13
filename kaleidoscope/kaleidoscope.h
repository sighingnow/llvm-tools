#ifndef __LLVM_KALEIDOSCOPE_KALEIDOSCOPE_H__
#define __LLVM_KALEIDOSCOPE_KALEIDOSCOPE_H__

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <string>
#include <variant>
#include <vector>

#include <fmt/format.h>
#include <fmt/ranges.h>

#include <boost/spirit/include/qi.hpp>

#include <llvm/ADT/StringRef.h>
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
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>

namespace qi = boost::spirit::qi;
namespace orc = llvm::orc;

template <class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...)->overloaded<Ts...>;

template <typename Iter, typename P, typename... Args>
bool parse_and_check(Iter begin, Iter end, P const &p, Args &&... args) {
    bool ok = qi::parse(begin, end, p, std::forward<Args>(args)...);
    if (!ok || begin != end) {
        throw std::runtime_error(fmt::format("parse error: '{}'", std::string(begin, end)));
    }
    return ok;
}

template <typename Iter, typename P, typename Skip, typename... Args>
bool phrase_parse_and_check(Iter begin, Iter end, P const &p, Skip const &skip, Args &&... args) {
    bool ok = qi::phrase_parse(begin, end, p, skip, std::forward<Args>(args)...);
    if (!ok || begin != end) {
        throw std::runtime_error(fmt::format("phrase parse error: '{}'", std::string(begin, end)));
    }
    return ok;
}

struct expr_t;
struct number_t;
struct variable_t;
struct unary_expr_t;
struct binary_expr_t;
struct call_t;
struct branch_t;
struct prototype_t;
struct function_t;
struct program_t;
struct procedure_t;

// Note that here std::unique is not supported by Boost's value initialization.
namespace detail {
template <typename T>
using ast_ptr_t = std::shared_ptr<T>;

template <typename T, typename... Args>
inline auto ast_construct(Args &&... args);
}  // namespace detail

using expr_ptr_t = detail::ast_ptr_t<expr_t>;
using number_ptr_t = detail::ast_ptr_t<number_t>;
using variable_ptr_t = detail::ast_ptr_t<variable_t>;
using unary_expr_ptr_t = detail::ast_ptr_t<unary_expr_t>;
using binary_expr_ptr_t = detail::ast_ptr_t<binary_expr_t>;
using call_ptr_t = detail::ast_ptr_t<call_t>;
using branch_ptr_t = detail::ast_ptr_t<branch_t>;
using prototype_ptr_t = detail::ast_ptr_t<prototype_t>;
using function_ptr_t = detail::ast_ptr_t<function_t>;
using program_ptr_t = detail::ast_ptr_t<program_t>;
using procedure_ptr_t = detail::ast_ptr_t<procedure_t>;

struct scope_t {
    scope_t() {
        scopes.push_back(dict_t());  // top-level
    }
    void enter() {
        scopes.push_back(dict_t());
    }
    void exit() {
        if (scopes.empty()) {
            throw std::runtime_error("Cannot exit at outside of top-level");
        }
        scopes.pop_back();
    }
    void witness(std::string const &name, llvm::Value *value) {
        scopes.rbegin()->emplace(name, value);  // FIXME: needs validation
    }
    void witnessFunction(std::string const &name, prototype_ptr_t Function) {
        functionMap[name] = Function;
    }
    prototype_ptr_t function(std::string const &name) {
        return functionMap[name];
    }

    llvm::Value *local(std::string const &name);
    llvm::Value *global(std::string const &name);

    struct RTTI {
        RTTI(scope_t &scope) : scope(scope) {
            scope.enter();
        }
        ~RTTI() {
            scope.exit();
        }

       private:
        scope_t &scope;
    };

   private:
    using dict_t = std::map<std::string, llvm::Value *>;
    std::vector<dict_t> scopes;
    std::map<std::string, prototype_ptr_t> functionMap;
};

struct expr_t {
    expr_t() = default;
    virtual std::string format() const = 0;
    virtual llvm::Value *codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                                 scope_t &) const = 0;
};

struct number_t : public expr_t {
    number_t(double const &val) : val(val) {
    }
    std::string format() const override {
        return std::to_string(val);
    }
    virtual llvm::Value *codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                                 scope_t &) const override;

   private:
    double val;
};

struct variable_t : public expr_t {
    variable_t(std::string const &name) : name(name) {
    }
    std::string format() const override {
        return name;
    }
    virtual llvm::Value *codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                                 scope_t &) const override;
    std::string const &getName() const noexcept {
        return name;
    }

   private:
    std::string name;
};

struct unary_expr_t : public expr_t {
    unary_expr_t(char const &op, expr_ptr_t const &operand) : op(op), operand(operand) {
    }
    std::string format() const override {
        return fmt::format("{} {}", op, operand->format());
    }
    virtual llvm::Value *codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                                 scope_t &) const override;

   private:
    char op;
    expr_ptr_t operand;
};

struct binary_expr_t : public expr_t {
    binary_expr_t(expr_ptr_t const &lhs, char const &op, expr_ptr_t const &rhs)
            : lhs(lhs), op(op), rhs(rhs) {
    }
    binary_expr_t(expr_ptr_t const &lhs, std::vector<std::pair<char, expr_ptr_t>> const &rhss)
            : lhs(lhs) {
        for (size_t i = 0; i < rhss.size() - 1; ++i) {
            this->lhs = std::make_shared<binary_expr_t>(this->lhs, rhss[i].first, rhss[i].second);
        }
        this->op = rhss.rbegin()->first;
        this->rhs = rhss.rbegin()->second;
    }
    binary_expr_t(expr_ptr_t const &lhs, std::pair<char, expr_ptr_t> const &rhs)
            : lhs(lhs), op(rhs.first), rhs(rhs.second) {
    }
    std::string format() const override {
        return fmt::format("({} {} {})", lhs->format(), op, rhs->format());
    }
    virtual llvm::Value *codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                                 scope_t &) const override;

   private:
    expr_ptr_t lhs;
    char op;
    expr_ptr_t rhs;
};

struct call_t : public expr_t {
    call_t(std::string const &callee, std::vector<expr_ptr_t> const &args)
            : callee(callee), args(args) {
    }
    std::string format() const override {
        std::vector<std::string> fmtargs(args.size());
        std::transform(args.begin(), args.end(), fmtargs.begin(),
                       [](auto &&arg) { return arg->format(); });
        return fmt::format("{}({})", callee, fmtargs);
    }
    virtual llvm::Value *codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                                 scope_t &) const override;

   private:
    std::string callee;
    std::vector<expr_ptr_t> args;
};

struct branch_t : public expr_t {
    branch_t(expr_ptr_t const &condition, expr_ptr_t const &ifbody, expr_ptr_t const &elsebody)
            : condition(condition), ifbody(ifbody), elsebody(elsebody) {
    }
    std::string format() const override {
        return fmt::format("(({}) ? ({}) : ({}))", condition->format(), ifbody->format(),
                           elsebody->format());
    }
    virtual llvm::Value *codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                                 scope_t &) const override;

   private:
    expr_ptr_t condition, ifbody, elsebody;
};

struct prototype_t {
    prototype_t(std::string const &name, std::vector<variable_ptr_t> const &args)
            : name(name), args(args) {
    }
    std::string format() const {
        std::vector<std::string> fmtargs(args.size());
        std::transform(args.begin(), args.end(), fmtargs.begin(),
                       [](auto &&arg) { return arg->format(); });
        return fmt::format("{}({});", name, fmtargs);
    }
    llvm::Function *codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                            scope_t &) const;
    std::string const &getName() const noexcept {
        return name;
    }

   private:
    std::string name;
    std::vector<variable_ptr_t> args;
};

struct function_t {
    function_t(std::string const &name, std::vector<variable_ptr_t> const &args,
               expr_ptr_t const &body)
            : prototype(std::make_shared<prototype_t>(name, args)), body(body) {
    }
    function_t(prototype_ptr_t const &prototype, expr_ptr_t const &body)
            : prototype(prototype), body(body) {
    }
    std::string format() const {
        return fmt::format("{}\{\n{}\n\}", prototype->format(), body->format());
    }
    llvm::Function *codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                            scope_t &) const;

   private:
    prototype_ptr_t prototype;
    expr_ptr_t body;
};

struct program_t {
    program_t(expr_ptr_t const &body) : body(body) {
    }
    program_t(std::vector<prototype_ptr_t> const &prototypes, expr_ptr_t const &body)
            : prototypes(prototypes), body(body) {
    }
    program_t(std::vector<prototype_ptr_t> const &prototypes,
              std::vector<function_ptr_t> const &functions, expr_ptr_t const &body)
            : prototypes(prototypes), functions(functions), body(body) {
    }
    std::string format() const {
        std::vector<std::string> fmtprotos(prototypes.size()), fmtfuncs(functions.size());
        std::transform(prototypes.begin(), prototypes.end(), fmtprotos.begin(),
                       [](auto &&p) { return p->format(); });
        std::transform(functions.begin(), functions.end(), fmtfuncs.begin(),
                       [](auto &&p) { return p->format(); });
        return fmt::format("Prototypes:\n{}\n\n Functions:\n{}\n\n Body:\n{}", fmtprotos, fmtfuncs,
                           body->format());
    }
    std::vector<llvm::Function *> codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                                          scope_t &) const;

   private:
    std::vector<prototype_ptr_t> prototypes;
    std::vector<function_ptr_t> functions;
    expr_ptr_t body;
};

struct procedure_t {
    procedure_t(prototype_ptr_t const &prototype) : prog(prototype) {
    }
    procedure_t(function_ptr_t const &function) : prog(function) {
    }
    procedure_t(expr_ptr_t const &body) : prog(body) {
    }
    std::string format() const {
        return std::visit(overloaded{[](prototype_ptr_t const &p) {
                                         return fmt::format("Prototype: {}", p->format());
                                     },
                                     [](function_ptr_t const &f) {
                                         return fmt::format("Function: {}", f->format());
                                     },
                                     [](expr_ptr_t const &e) {
                                         return fmt::format("Expression: {}", e->format());
                                     }},
                          this->prog);
    }
    llvm::Function *codegen(llvm::Module *, llvm::LLVMContext &, llvm::IRBuilder<> &,
                            scope_t &) const;

   private:
    std::variant<prototype_ptr_t, function_ptr_t, expr_ptr_t> prog;
};

class grammar_rules {
   public:
    grammar_rules();

   protected:
    template <typename R>
    using rule_t = qi::rule<std::string::const_iterator, R(), qi::space_type>;

    rule_t<std::string> identifier;
    rule_t<procedure_ptr_t> procedure;
    rule_t<program_ptr_t> program;
    rule_t<prototype_ptr_t> prototype;
    rule_t<function_ptr_t> function;
    rule_t<std::vector<expr_ptr_t>> ts;
    rule_t<variable_ptr_t> variable;
    rule_t<expr_ptr_t> expr, primary, number, group, call, branch, unary, binary;
    // Operator precedence parser: https://en.wikipedia.org/wiki/Operator-precedence_parser
    rule_t<expr_ptr_t> cmp_exp, add_exp, mul_exp;
    rule_t<std::pair<char, expr_ptr_t>> cmp_many, add_many, mul_many;
};

class program_grammar
        : public grammar_rules,
          public qi::grammar<std::string::const_iterator, program_ptr_t(), qi::space_type> {
   public:
    program_grammar();
};

class procedure_grammar
        : public grammar_rules,
          public qi::grammar<std::string::const_iterator, procedure_ptr_t(), qi::space_type> {
   public:
    procedure_grammar();
};

class JIT {
   public:
    JIT(llvm::DataLayout &&Layout, orc::JITTargetMachineBuilder &&JTMB)
            : Layout(std::move(Layout)),
              LinkLayer(ES, []() { return llvm::make_unique<llvm::SectionMemoryManager>(); }),
              CompileLayer(ES, LinkLayer, orc::ConcurrentIRCompiler(std::move(JTMB))),
              TransformLayer(ES, CompileLayer, optimizer),
              Mangle(ES, this->Layout),
              JITContext(std::make_unique<llvm::LLVMContext>()) {
        ES.getMainJITDylib().setGenerator(llvm::cantFail(
                orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(this->Layout)));
    }

    static llvm::Expected<std::unique_ptr<JIT>> Create();

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
    orc::ThreadSafeContext JITContext;
};

#endif
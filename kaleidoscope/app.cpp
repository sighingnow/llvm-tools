#include <algorithm>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>

#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <fmt/format.h>
#include <fmt/ranges.h>

namespace qi = boost::spirit::qi;
namespace phx = boost::phoenix;

template <typename Iter, typename P, typename... Args>
bool parse_and_check(Iter begin, Iter end, P const & p, Args && ... args) {
    bool ok = qi::parse(begin, end, p, std::forward<Args>(args)...);
    if (!ok || begin != end) {
        throw std::runtime_error(fmt::format("parse error: '{}'", std::string(begin, end)));
    }
    return ok;
}

template <typename Iter, typename P, typename Skip, typename... Args>
bool phrase_parse_and_check(Iter begin, Iter end, P const & p, Skip const & skip, Args && ... args) {
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

// Note that here std::unique is not supported by Boost's value initialization.
namespace detail {
    template <typename T>
    using ast_ptr_t = std::shared_ptr<T>;

    template <typename T, typename... Args>
    inline auto ast_construct(Args &&... args) {
        return phx::construct<ast_ptr_t<T>>(phx::new_<T>(std::forward<Args>(args)...));
    }
}

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

struct expr_t {
    expr_t() = default;
    virtual std::string format() const = 0;
};

struct number_t: public expr_t {
    number_t(double const & val): val(val) {}
    std::string format() const override {
        return std::to_string(val);
    }
  private:
    double val;
};

struct variable_t: public expr_t {
    variable_t(std::string const & name): name(name) {}
    std::string format() const override {
        return name;
    }
  private:
    std::string name;
};

struct unary_expr_t: public expr_t {
    unary_expr_t(char const & op, expr_ptr_t const & operand): op(op), operand(operand) {}
    std::string format() const override {
        return fmt::format("{} {}", op, operand->format());
    }
  private:
    char op;
    expr_ptr_t operand;
};

struct binary_expr_t: public expr_t {
    binary_expr_t(expr_ptr_t const & lhs, char const & op, expr_ptr_t const & rhs): lhs(lhs), op(op), rhs(rhs) {}
    binary_expr_t(expr_ptr_t const & lhs, std::vector<std::pair<char, expr_ptr_t>> const & rhss): lhs(lhs) {
        for (size_t i = 0; i < rhss.size() - 1; ++i) {
            this->lhs = std::make_shared<binary_expr_t>(this->lhs, rhss[i].first, rhss[i].second);
        }
        this->op = rhss.rbegin()->first;
        this->rhs = rhss.rbegin()->second;
    }
    binary_expr_t(expr_ptr_t const & lhs, std::pair<char, expr_ptr_t> const & rhs): lhs(lhs), op(rhs.first), rhs(rhs.second) {}
    std::string format() const override {
        return fmt::format("({} {} {})", lhs->format(), op, rhs->format());
    }
  private:
    expr_ptr_t lhs;
    char op;
    expr_ptr_t rhs;
};

struct call_t: public expr_t {
    call_t(std::string const & callee, std::vector<expr_ptr_t> const & args): callee(callee), args(args) {}
    std::string format() const override {
        std::vector<std::string> fmtargs(args.size());
        std::transform(args.begin(), args.end(), fmtargs.begin(), [](auto && arg) { return arg->format(); });
        return fmt::format("{}({})", callee, fmtargs);
    }
  private:
    std::string callee;
    std::vector<expr_ptr_t> args;
};

struct branch_t: public expr_t {
    branch_t(expr_ptr_t const & condition, expr_ptr_t const & ifbody, expr_ptr_t const & elsebody):
        condition(condition), ifbody(ifbody), elsebody(elsebody) {}
    std::string format() const override {
        return fmt::format("(({}) ? ({}) : ({}))", condition->format(), ifbody->format(), elsebody->format());
    }
  private:
    expr_ptr_t condition, ifbody, elsebody;
};

struct prototype_t {
    prototype_t(std::string const & name, std::vector<variable_ptr_t> const & args): name(name), args(args) {}
    std::string format() const {
        std::vector<std::string> fmtargs(args.size());
        std::transform(args.begin(), args.end(), fmtargs.begin(), [](auto && arg) { return arg->format(); });
        return fmt::format("{}({});", name, fmtargs);
    }
  private:
    std::string name;
    std::vector<variable_ptr_t> args;
};

struct function_t {
    function_t(std::string const & name, std::vector<variable_ptr_t> const & args, expr_ptr_t const & body):
        prototype(std::make_shared<prototype_t>(name, args)), body(body) {}
    function_t(prototype_ptr_t const & prototype, expr_ptr_t const & body): prototype(prototype), body(body) {}
    std::string format() const {
        return fmt::format("{}\{\n{}\n\}", prototype->format(), body->format());
    }
  private:
    prototype_ptr_t prototype;
    expr_ptr_t body;
};

struct program_t {
    program_t(expr_ptr_t const & body): body(body) {}
    program_t(std::vector<prototype_ptr_t> const & prototypes,
              expr_ptr_t const & body): prototypes(prototypes), body(body) {}
    program_t(std::vector<prototype_ptr_t> const & prototypes,
              std::vector<function_ptr_t> const & functions,
              expr_ptr_t const & body): prototypes(prototypes), functions(functions), body(body) {}
    std::string format() const {
        std::vector<std::string> fmtprotos(prototypes.size()), fmtfuncs(functions.size());
        std::transform(prototypes.begin(), prototypes.end(), fmtprotos.begin(), [](auto && p) { return p->format(); });
        std::transform(functions.begin(), functions.end(), fmtfuncs.begin(), [](auto && p) { return p->format(); });
        return fmt::format("Prototypes:\n{}\n\n Functions:\n{}\n\n Body:\n{}", fmtprotos, fmtfuncs, body->format());
    }
  private:
    std::vector<prototype_ptr_t> prototypes;
    std::vector<function_ptr_t> functions;
    expr_ptr_t body;
};

class grammar_t: public qi::grammar<std::string::const_iterator, program_ptr_t(), qi::space_type> {
  public:
    grammar_t(): grammar_t::base_type(program) {
        // program:
        program     = (*prototype >> *function >> expr)
                      [ qi::_val = detail::ast_construct<program_t>(qi::_1, qi::_2, qi::_3) ];
        // helpers:
        identifier %= qi::alpha >> *qi::alnum;
        // expr:
        expr       %= unary
                    | cmp_exp
                    | primary;
        primary    %= group
                    | branch
                    | call
                    | variable
                    | number;
        // number := double
        number      = qi::double_
                      [ qi::_val = detail::ast_construct<number_t>(qi::_1) ];
        // group := '(' expr ')'
        group      %= '(' >> expr >> ')';
        // variable := identifier;
        variable    = identifier
                      [ qi::_val = detail::ast_construct<variable_t>(qi::_1) ];
        // unary := ('!' | '-') expr
        unary       = ((qi::char_('!') | qi::char_('-')) >> expr)
                      [ qi::_val = detail::ast_construct<unary_expr_t>(qi::_1, qi::_2) ];
        // mul := expr ( ('*' | '/') expr )+
        mul_many    = ((qi::char_('*') | qi::char_('/')) >> primary)
                      [ qi::_val = phx::construct<std::pair<char, expr_ptr_t>>(qi::_1, qi::_2) ];
        mul_exp     = (primary >> mul_many)
                      [ qi::_val = detail::ast_construct<binary_expr_t>(qi::_1, qi::_2) ]
                    | primary
                      [ qi::_val = qi::_1 ];
        // add := expr ( ('+' | '-') expr )+
        add_many    = ((qi::char_('+') | qi::char_('-')) >> mul_exp)
                      [ qi::_val = phx::construct<std::pair<char, expr_ptr_t>>(qi::_1, qi::_2) ];
        add_exp     = (mul_exp >> +add_many)
                      [ qi::_val = detail::ast_construct<binary_expr_t>(qi::_1, qi::_2) ]
                    | mul_exp
                      [ qi::_val = qi::_1 ];
        // cmp := expr ('>' | '<' | '=') expr
        cmp_exp     = (add_exp >> (qi::char_('>') | qi::char_('<') | qi::char_('=')) >> add_exp)
                      [ qi::_val = detail::ast_construct<binary_expr_t>(qi::_1, qi::_2, qi::_3) ]
                    | add_exp
                      [ qi::_val = qi::_1 ];
        // call := identifier '(' expr* ')'
        call        = (identifier >> '(' >> (expr % ',') >> ')')
                      [ qi::_val = detail::ast_construct<call_t>(qi::_1, qi::_2) ];
        // branch := 'if' expr '{' expr '}' 'else' '{' expr '}'
        branch      = ("if" >> primary >> '{' >> expr >> '}' >> "else" >> '{' >> expr >> '}')
                      [ qi::_val = detail::ast_construct<branch_t>(qi::_1, qi::_2, qi::_3) ];
        // prototype := 'extern' identifier '(' expr* ')' ';'
        prototype   = ("extern" >> identifier >> '(' >> (variable % ',') >> ')' >> ';')
                      [ qi::_val = detail::ast_construct<prototype_t>(qi::_1, qi::_2) ];
        // function := 'def' identifier '(' expr* ')' '{' expr '}'
        function    = ("def" >> identifier >> '(' >> (variable % ',') >> ')' >> '{' >> expr >> '}')
                      [ qi::_val = detail::ast_construct<function_t>(qi::_1, qi::_2, qi::_3) ];
    }
  private:
    template <typename R>
    using rule_t = qi::rule<std::string::const_iterator, R(), qi::space_type>;

    rule_t<std::string> identifier;
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

void test() {
    std::string ts[] = {
        R"( 1.5 )",
        R"( (1.5) )",
        R"( t )",
        R"( (t) )",
        R"( t(1, 2, 3) )",
        R"( 1 * 2 )",
        R"( 1+2-3/4+6/5 )",
        R"( 1+(2-3)/4+6/5 )",
        R"( extern fs(a, b, c);
            fs(1, 2, 3)
          )",
        R"( if (1) { fs(4, 5, 6) } else { fs(7, 8, 9) }
          )",
    };
    program_ptr_t r;
    for (auto && s: ts) {
        phrase_parse_and_check(s.cbegin(), s.cend(), grammar_t(), qi::space, r);
        fmt::print("Got: {}\n", r->format());
    }
}

int main(int argc, char **argv) {
    test();
}

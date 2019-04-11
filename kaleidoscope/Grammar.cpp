#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include "kaleidoscope.h"

namespace qi = boost::spirit::qi;
namespace phx = boost::phoenix;

// Note that here std::unique is not supported by Boost's value initialization.
namespace detail {
    template <typename T, typename... Args>
    inline auto ast_construct(Args &&... args) {
        return phx::construct<ast_ptr_t<T>>(phx::new_<T>(std::forward<Args>(args)...));
    }
}

grammar_rules::grammar_rules() {
    // procedure:
    procedure   = prototype
                  [ qi::_val = detail::ast_construct<procedure_t>(qi::_1) ]
                | function
                  [ qi::_val = detail::ast_construct<procedure_t>(qi::_1) ]
                | expr
                  [ qi::_val = detail::ast_construct<procedure_t>(qi::_1) ];
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

program_grammar::program_grammar(): program_grammar::base_type(program) {
}

procedure_grammar::procedure_grammar(): procedure_grammar::base_type(procedure) {
}

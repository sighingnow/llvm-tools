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
struct sum_t;
struct product_t;
struct unit_t;
struct group_t;

// Note that here std::unique is not supported by Boost's value initialization.
using expr_ptr_t = std::shared_ptr<expr_t>;
using sum_ptr_t = std::shared_ptr<sum_t>;
using product_ptr_t = std::shared_ptr<product_t>;
using unit_ptr_t = std::shared_ptr<unit_t>;
using group_ptr_t = std::shared_ptr<group_t>;

struct expr_t {
    expr_t() = default;
    virtual std::string format() const = 0;
};

struct sum_t: public expr_t {
    expr_ptr_t lhs, rhs;
    sum_t(expr_ptr_t lhs, expr_ptr_t rhs): lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    virtual std::string format() const override {
        return fmt::format("({} + {})", lhs->format(), rhs->format());
    }
};

struct product_t: public expr_t {
    expr_ptr_t lhs, rhs;
    product_t(expr_ptr_t lhs, expr_ptr_t rhs): lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    virtual std::string format() const override {
        return fmt::format("({} * {})", lhs->format(), rhs->format());
    }
};

struct unit_t: public expr_t {
    int val;
    unit_t() {
        throw std::runtime_error("impossible");
    };
    unit_t(int val): val(val) {}
    virtual std::string format() const override {
        return fmt::format("{}", val);
    }
};

class grammar_t: public qi::grammar<std::string::const_iterator, expr_ptr_t(), qi::space_type> {
  public:
    grammar_t(): grammar_t::base_type(sum) {
        // sum     = product [qi::_val = qi::_1] >> '+' >> sum [qi::_val += qi::_1]
        //         | product [qi::_val = qi::_1];
        // product = unit [qi::_val = qi::_1] >> '*' >> product [qi::_val *= qi::_1]
        //         | unit [qi::_val = qi::_1];
        // unit   %= qi::int_ | group;
        // group  %= '(' >> sum >> ')';

        sum = (product >> '+' >> sum) [qi::_val = phx::construct<sum_ptr_t>(phx::new_<sum_t>(qi::_1, qi::_2))]
            | product [qi::_val = qi::_1];
        product = (unit >> '*' >> product) [qi::_val = phx::construct<product_ptr_t>(phx::new_<product_t>(qi::_1, qi::_2))]
            | unit [qi::_val = qi::_1];
        unit = group [qi::_val = qi::_1]
            | qi::int_ [qi::_val = phx::construct<unit_ptr_t>(phx::new_<unit_t>(qi::_1))];
        group %= '(' >> sum >> ')';
    }
  private:
    qi::rule<std::string::const_iterator, expr_ptr_t(), qi::space_type> sum, product, unit, group;
};

void test() {
    std::string t1 = "1 + 2 * 3 + (4 + 1) * 5";
    expr_ptr_t r;
    phrase_parse_and_check(t1.cbegin(), t1.cend(), grammar_t(), qi::space, r);
    fmt::print("Got: {}", r->format());
}

int main(int argc, char **argv) {
    test();
}

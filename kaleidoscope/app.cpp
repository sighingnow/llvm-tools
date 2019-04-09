#include "kaleidoscope.h"

void test() {
    std::string ts[] = {
        R"( 1.5 )",
        R"( (1.5) )",
        R"( 1 * 2 )",
        R"( 1+2-3/4+6/5 )",
        R"( 1+(2-3)/4+6/5 )",
        R"( if (1 < 2) { 3 } else { 4 } )",
        R"( extern max(a, b);
            extern min(a, b);
            def fs(a, b, c) { a + (b-c) / a + a * b - c + max(a, b) - min(a, c) }
            fs(1, 2, 3) + if (1 = 1) { fs(4, 5, 6) } else { fs(7, 8, 9) }
          )",
        R"( extern fs(a, b, c);
            fs(1, 2, 3)
          )",
    };
    for (auto && s: ts) {
        program_ptr_t r;
        phrase_parse_and_check(s.cbegin(), s.cend(), grammar_t(), qi::space, r);
        // fmt::print("Got: {}\n", r->format());
        r->codegen();
    }
}

void test_args(int argc, char **argv) {
    for (size_t i = 1; i < argc; ++i) {
        std::string s(argv[i]);
        program_ptr_t r;
        phrase_parse_and_check(s.cbegin(), s.cend(), grammar_t(), qi::space, r);
        // fmt::print("Got: {}\n", r->format());
        r->codegen();
    }
}

int main(int argc, char **argv) {
    if (argc <= 1) {
        test();
    } else {
        test_args(argc, argv);
    }
}

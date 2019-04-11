#include <readline/readline.h>

#include "kaleidoscope.h"

static std::string ts[] = {
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

int repl(int, char **) {
    llvm::LLVMContext Context;
    llvm::IRBuilder<> Builder(Context);
    llvm::Module Module("kaleidoscope", Context);
    scope_t Scope;

    std::string prompt = "> ";

    while (true) {
        char *buffer = readline(prompt.c_str());
        std::string ss(buffer);
        free(buffer);
        if (ss.compare(0, 2, ":q") == 0) {
            break;
        }
        procedure_ptr_t r;
        phrase_parse_and_check(ss.cbegin(), ss.cend(), procedure_grammar(), qi::space, r);
        r->codegen(Module, Context, Builder, Scope);
    }
    return 0;
}

int main(int argc, char **argv) {
    return repl(argc, argv);
}

#include <readline/readline.h>
#include <cassert>

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
    std::string prompt = "> ";

    orc::ThreadSafeContext JITContext(std::make_unique<llvm::LLVMContext>());
    std::unique_ptr<JIT> TheJIT
            = llvm::cantFail(JIT::Create(JITContext), "Unable to create the JIT engine");
    llvm::IRBuilder<> Builder(*JITContext.getContext());
    scope_t Scope;

    int round = 0;

    while (true) {
        char *buffer = readline(prompt.c_str());
        std::string ss(buffer);
        free(buffer);
        if (ss.compare(0, 2, ":q") == 0 || ss.compare(0, 4, "exit") == 0
            || ss.compare(0, 4, "quit") == 0) {
            break;
        }
        if (std::all_of(ss.begin(), ss.end(), [](char const &ch) { return std::isspace(ch); })) {
            continue;
        }

        // NB: DONT use the same name for the moduleID and the entry function.
        std::string modname = fmt::format("kaleidoscope_{}", round++);
        std::string entry = fmt::format("{}_entry", modname);

        procedure_ptr_t r;
        try {
            phrase_parse_and_check(ss.cbegin(), ss.cend(), procedure_grammar(), qi::space, r);
        } catch (std::exception const &e) {
            fmt::print(stderr, "failed to parse the input: \n\t{}\n", e.what());
            continue;
        }
        auto Module = std::make_unique<llvm::Module>(modname, *JITContext.getContext());
        llvm::Function *f = nullptr;
        try {
            f = r->codegen(Module.get(), *JITContext.getContext(), Builder, Scope);
        } catch (std::exception const &e) {
            fmt::print(stderr, "failed to generate llvm ir: \n\t{}\n", e.what());
            continue;
        }
        try {
            TheJIT->addIRModule(std::move(Module));
        } catch (std::exception const &e) {
            fmt::print(stderr, "failed to jit: \n\t{}\n", e.what());
            continue;
        }

        if (f->getName() == entry) {
            auto syn = llvm::cantFail(TheJIT->lookup(entry));
            auto fptr = (double (*)())(uintptr_t)syn.getAddress();
            fprintf(stdout, "res: %f\n", fptr());
        }
    }
    return 0;
}

int main(int argc, char **argv) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    return repl(argc, argv);
}

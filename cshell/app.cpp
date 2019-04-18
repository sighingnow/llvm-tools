#include <readline/readline.h>

#include "Common.h"
#include "Frontend.h"
#include "JIT.h"

int repl(int argc, char const **argv) {
    // Prepare for repl
    std::string prompt = "> ";
    int round = 0;

    auto TheContext = llvm::orc::ThreadSafeContext(std::make_unique<llvm::LLVMContext>());
    Frontend TheFrontend(TheContext.getContext());
    auto TheJIT = llvm::cantFail(JIT::Create(TheContext), "Unable to create the JIT engine");

    while (true) {
        char *buffer = readline(prompt.c_str());
        std::string Code(buffer);
        free(buffer);
        if (Code.compare(0, 2, ":q") == 0 || Code.compare(0, 4, "exit") == 0
            || Code.compare(0, 4, "quit") == 0) {
            break;
        }
        if (std::all_of(Code.begin(), Code.end(),
                        [](char const &ch) { return std::isspace(ch); })) {
            continue;
        }

        std::string FileName = fmt::format("interactive[{}]", round++);
        auto Module = TheFrontend.runCode(Code, FileName);

        if (Module) {
            TheJIT->addIRModule(std::move(Module));
            auto syn = TheJIT->lookup("main");
            if (!syn) {
                discardError(std::move(syn), false);
            } else {
                auto fptr = (int (*)())(uintptr_t)syn.get().getAddress();
                fprintf(stdout, "res: %d\n", fptr());
            }
        }
    }

    // Shutdown.
    llvm::llvm_shutdown();
    return 0;
}

int main(int argc, char const **argv) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    return repl(argc, argv);
}

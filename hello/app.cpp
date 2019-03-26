#include <iostream>

#include <clang/Config/config.h>
#include <llvm/Config/llvm-config.h>

int main() {
    std::cout << "LLVM: " BACKEND_PACKAGE_STRING << std::endl;
}
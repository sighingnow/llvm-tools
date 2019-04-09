add_llvm_executable(kaleidoscope
    app.cpp
    Codegen.cpp
    Grammar.cpp
)
target_link_libraries(kaleidoscope PUBLIC
    LLVMCore
    LLVMSupport
)
setup_build_context(kaleidoscope)

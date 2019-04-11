add_llvm_executable(kaleidoscope
    app.cpp
    Codegen.cpp
    Grammar.cpp
)
target_link_libraries(kaleidoscope PUBLIC
    LLVMCore
    LLVMSupport
)
target_link_libraries(kaleidoscope PRIVATE readline)
setup_build_context(kaleidoscope)

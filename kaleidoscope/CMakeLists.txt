add_llvm_executable(kaleidoscope
    app.cpp
)
target_link_libraries(kaleidoscope PUBLIC
    LLVMCore
    LLVMSupport
)
setup_build_context(kaleidoscope)

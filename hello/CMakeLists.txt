add_llvm_executable(hello
    app.cpp
)
target_link_libraries(hello PUBLIC
    clangAST
    clangFrontend
    clangTooling
    clangRewrite
    clangSerialization
    LLVMSupport
)
setup_build_context(hello)

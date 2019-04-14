add_llvm_executable(hello
    app.cpp
)
target_link_libraries(hello PUBLIC
    -Wl,--start-group
    clangAnalysis
    clangAST
    clangBasic
    clangDriver
    clangEdit
    clangFrontend
    clangLex
    clangTooling
    clangParse
    clangRewrite
    clangSema
    clangSerialization
    LLVMBitWriter
    LLVMOption
    LLVMSupport
    -Wl,--end-group
)
if (WIN32 AND MINGW)
    # Fix the undefined reference issue with MinGW-w64
    target_link_libraries(hello PUBLIC version)
endif ()
setup_build_context(hello)

add_llvm_executable(cshell
    Frontend.cpp
    JIT.cpp
    app.cpp
)
target_link_libraries(cshell PUBLIC
    -Wl,--start-group
    clangAnalysis
    clangAST
    clangBasic
    clangCodeGen
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
    LLVMCore
    LLVMExecutionEngine
    LLVMInstCombine
    LLVMObject
    LLVMOption
    LLVMOrcJIT
    LLVMScalarOpts
    LLVMSupport
    LLVMTarget
    LLVMX86AsmPrinter
    LLVMX86CodeGen
    LLVMX86Desc
    LLVMX86Info
    -Wl,--end-group
)
target_link_libraries(cshell PRIVATE readline)
if (WIN32 AND MINGW)
    # Fix the undefined reference issue with MinGW-w64
    target_link_libraries(cshell PUBLIC version)
endif ()
setup_build_context(cshell)

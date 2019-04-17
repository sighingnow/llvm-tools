add_llvm_executable(kaleidoscope
    app.cpp
    Codegen.cpp
    Grammar.cpp
    JIT.cpp
)
target_link_libraries(kaleidoscope PUBLIC
    LLVMCore
    LLVMExecutionEngine
    LLVMInstCombine
    LLVMObject
    LLVMOrcJIT
    LLVMScalarOpts
    LLVMSupport
    LLVMTarget
    LLVMX86CodeGen
    LLVMX86Desc
    LLVMX86Info
    LLVMX86AsmPrinter
)
target_link_libraries(kaleidoscope PRIVATE readline)
setup_build_context(kaleidoscope)

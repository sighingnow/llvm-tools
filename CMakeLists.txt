cmake_minimum_required(VERSION 3.8 FATAL_ERROR)

project(hello-llvm LANGUAGES C CXX)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()
message(STATUS "Build with ${CMAKE_BUILD_TYPE} mode")

find_package(LLVM)
list(APPEND CMAKE_MODULE_PATH "${LLVM_DIR}")
message(STATUS "LLVM_DIR: ${LLVM_DIR}, version ${LLVM_PACKAGE_VERSION}")

include(AddLLVM)
include(HandleLLVMOptions)

add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

include(third_party/cotire/CMake/cotire.cmake)

function(setup_build_context APP)
    target_compile_features(${APP} PUBLIC cxx_std_17)

    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_compile_options(${APP} PUBLIC -O0 -g -ggdb)
        target_compile_definitions(${APP} PUBLIC DEBUG)
    else()
        target_compile_options(${APP} PUBLIC -O2 -march=native)
        target_compile_definitions(${APP} PUBLIC NDEBUG)
    endif()

    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
        target_compile_options(${APP}
            PRIVATE -Wall
                    -Wextra
                    -Wconversion
                    -pedantic
                    -Wfatal-errors
        )
    endif ()

    set_property(TARGET ${APP} PROPERTY RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)

    # Enable cotire
    cotire(${APP})
endfunction()

add_subdirectory(hello)

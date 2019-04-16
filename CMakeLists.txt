cmake_minimum_required(VERSION 3.8 FATAL_ERROR)

project(hello-llvm LANGUAGES C CXX)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()
message(STATUS "Build with ${CMAKE_BUILD_TYPE} mode")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

find_package(LLVM)
list(APPEND CMAKE_MODULE_PATH "${LLVM_DIR}")
message(STATUS "LLVM_DIR: ${LLVM_DIR}, version ${LLVM_PACKAGE_VERSION}")

include(AddLLVM)
include(HandleLLVMOptions)

add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})

option(LLVM_ENABLE_DUMP "The LLVM distribution has enable the dump option" OFF)
if (LLVM_ENABLE_DUMP)
    add_definitions(-DLLVM_ENABLE_DUMP)
endif ()

# Checking the option in CMake GUI seems doesn't work.
set(LLVM_ENABLE_EH ON)
set(LLVM_ENABLE_RTTI ON)

set(Boost_USE_MULTITHREADED ON)
find_package(Boost REQUIRED)

include_directories(${Boost_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIRS})

add_subdirectory(third_party/fmt)
include_directories(third_party/fmt/include)
link_libraries(fmt::fmt)

include(third_party/cotire/CMake/cotire.cmake)

include(cmake/use-ccache.cmake)
include(cmake/use-ld-gold.cmake)

## Set up compiler and options.
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang")
    add_compile_options(
        -Wall
        -Wextra
        -pedantic-errors
        -Wfatal-errors
    )
endif ()

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    add_compile_options(-stdlib=libc++)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -stdlib=libc++ -lc++abi")
endif ()

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_definitions(-DDEBUG)
    add_compile_options(-O1 -g -ggdb)
    if (UNIX)
        add_compile_options(-fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer -fno-optimize-sibling-calls)
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fsanitize=address -fsanitize=undefined")
    endif ()
else ()
    add_definitions(-DNDEBUG)
    add_compile_options(-O2 -march=native)
endif ()

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

include_directories(support/include)

option(BUILD_HELLO OFF)
option(BUILD_KALEIDOSCOPE OFF)

if (BUILD_HELLO)
    add_subdirectory(hello)
endif ()

if (BUILD_KALEIDOSCOPE)
    add_subdirectory(kaleidoscope)
endif ()
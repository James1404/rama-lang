cmake_minimum_required(VERSION 3.27.7)

project(rama VERSION 0.01)

set(CMAKE_EXPORT_COMPILE_COMMANDS True)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(rama)

set(rama_CURRENT_DIR ${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory(${rama_CURRENT_DIR}/deps/fmt)

include(FetchContent)
FetchContent_Declare(
    argparse
    GIT_REPOSITORY https://github.com/p-ranav/argparse.git
)
FetchContent_MakeAvailable(argparse)

target_link_libraries(rama PUBLIC
    fmt::fmt
    argparse
)

file(GLOB rama_src
	${rama_CURRENT_DIR}/src/*.cpp
	${rama_CURRENT_DIR}/src/*.c
)


target_include_directories(rama PUBLIC
    ${rama_CURRENT_DIR}/include
)

target_sources(rama PRIVATE
    ${rama_src}
)

cmake_minimum_required(VERSION 3.10)
project(tuple)

set(CMAKE_CXX_COMPILER /usr/bin/g++)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
add_compile_options(-Wall -Wextra)

set(
    SOURCES
    include/const_integer.hpp
    include/const_map.hpp
    include/const_string.hpp
    include/function.hpp
    include/main.cpp
    include/template_magic.hpp

    examples/basic.cpp
)

add_executable(basic examples/basic.cpp)
set(EXAMPLES basic)
add_custom_target(examples)

foreach(target IN LISTS EXAMPLES)
    target_include_directories(${target} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
    add_dependencies(examples ${target})
endforeach()




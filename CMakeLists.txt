cmake_minimum_required(VERSION 3.10)
project(yet-another-nonstl)

set(CMAKE_CXX_COMPILER /usr/bin/clang++)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
add_compile_options(-Wno-gnu-string-literal-operator-template)

set(
    SOURCES
    basics.hpp
    const_integer.hpp
    const_map.hpp
    const_string.hpp
    function.hpp
    main.cpp
    template_magic.hpp
)

add_executable(main main.cpp)

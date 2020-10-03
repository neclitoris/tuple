cmake_minimum_required(VERSION 3.10)
project(tuple)

set(CMAKE_CXX_COMPILER /usr/bin/g++)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
add_compile_options(-Wall -Wextra)

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

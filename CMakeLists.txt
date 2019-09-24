set(CMAKE_MINIMUM_REQUIRED 3.10)
project(yet-another-nonstl)

set(CMAKE_CXX_COMPILER /Users/nikitasolo/.ya/tools/v3/425919040/bin/clang++)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(
    SOURCES
    basics.hpp
    const_integer.hpp
    const_map.hpp
    const_string.hpp
    main.cpp
    template_magic.hpp
)

add_executable(main main.cpp)

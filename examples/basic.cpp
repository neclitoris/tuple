#include "const_integer.hpp"
#include "const_map.hpp"
#include "const_string.hpp"

#include <concepts>
#include <iostream>

using namespace std::literals;

template <typename Map, typename T>
concept CanAccess = requires(const Map& m, T key) {
    { (void)m[key] } -> std::same_as<void>;
};

template <typename Map, typename T>
void try_print(const Map& m, T key)
    requires CanAccess<Map, T>
{
    std::cout << m[key] << std::endl;
}

template <typename Map, typename T>
void try_print(const Map&, T) {
    std::cout << "key unknown\n";
}

template <typename... Callables>
struct Overload : public Callables... {
    using Callables::operator()...;
};

int main() {
    // Basic usage: use operator""_ to create compile-time constants
    // to access the map. All necessary computations happen in compile time.
    const_map mp{(1_, "foo"), (2_, "bar"), ("baz"_, "cuux")};
    std::cout << mp[1_] << std::endl << mp[2_] << std::endl;

    // If map `m` has no key `k`, expression `m[k]` will result in a compile
    // error.
    try_print(mp, "baz"_);
    try_print(mp, 3_);

    // Keys also have equivalent runtime values that can be used to
    // access the map (with guaranteed O(1) complexity thanks to perfect
    // hashing). The return type is a variant though, so `visit` method may
    // prove a more convenient alternative to using `operator[]`.
    auto print =
        Overload{[&](const auto& v) { std::cout << v << std::endl; },
                 [&](std::monostate) { std::cout << "key unknown\n"; }};
    mp.visit("baz"sv, print);
    mp.visit(4, print);
    return 0;
}

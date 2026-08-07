#include "const_integer.hpp"
#include "const_map.hpp"
#include "const_string.hpp"

#include <iostream>

using namespace std::literals;

template <typename Map, typename T>
concept CanAccess = requires(const Map& m, T key) {
    { (void)m[key] };
};

template <typename Map, typename T>
void try_print(const Map& mp, T key)
    requires CanAccess<Map, T>
{
    std::cout << "mp[" << key << "]: " << mp[key] << "\n";
}

template <typename Map, typename T>
void try_print(const Map&, T key) {
    std::cout << "mp[" << key << "]: " << "no value" << "\n";
}

template <typename... Callables>
struct overload : public Callables... {
    using Callables::operator()...;
};

int main() {
    // Basic usage: use operator""_ to create compile-time constants
    // to access the map. All necessary computations happen in compile time.
    const_map mp{(1_, "foo"), (2_, "bar"), ("baz"_, "cuux")};
    std::cout << "mp[1]: " << mp[1_] << "\n"
              << "mp[2]: " << mp[2_] << "\n";

    // If map `m` has no key `k`, expression `m[k]` will result in a compile
    // error, but can be played around using overloads.
    try_print(mp, "baz"_);
    try_print(mp, 3_);

    // Keys also have equivalent runtime values that can be used to
    // access the map (with guaranteed O(1) complexity thanks to perfect
    // hashing). The return type is a variant though, so `visit` method may
    // prove a more convenient alternative to using `operator[]`.
    auto print = [](const auto& k) {
        return overload{[&](const auto& v) {
                            std::cout << "mp[" << k << "]: " << v << "\n";
                        },
                        [&](std::monostate) {
                            std::cout << "mp[" << k << "]: " << "no value\n";
                        }};
    };
    mp.visit("baz"sv, print("bar"sv));
    mp.visit(4, print(4));
    return 0;
}

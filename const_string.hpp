#pragma once

#include "const_map.hpp"
#include <cstdint>
#include <iostream>
#include <string_view>
#include <utility>

template <char... Sym>
struct const_string {
    static constexpr std::size_t count = sizeof...(Sym);

    static constexpr auto value = []() {
        static constexpr std::array<char, count> chars{Sym...};
        return std::string_view{chars.begin()};
    }();

    static constexpr std::array<std::byte, count> bytes = {(std::byte)Sym...};

    enum class integral : uint8_t {};

    explicit operator std::string_view() { return value; }

    friend std::ostream& operator<<(std::ostream& os, integral) {
        return os << value;
    }

    friend constexpr std::string_view to_string(integral) { return value; }

    template <typename T>
    friend constexpr auto operator,(integral i, T&& t) {
        return detail::const_map_getter{i, std::forward<T>(t)};
    }

    friend constexpr auto data(integral) { return data(value); };

    template <typename T>
        requires requires(T&& v) { std::string_view{std::forward<T>(v)}; }
    friend constexpr bool operator==(integral, T&& v) {
        return std::string_view{std::forward<T>(v)} == value;
    }
};

// use c++20 string literal operator template if possible
#if __cpp_nontype_template_parameter_class >= 201806
#include <array>
#include <compare>

namespace detail {
template <std::size_t n>
struct string {
    constexpr string(const char (&str)[n + 1]) {
        std::copy_n(str, n + 1, str_.begin());
    }

    friend constexpr auto operator<=>(const string&, const string&) = default;

    std::array<char, n + 1> str_;
};

template <std::size_t n>
string(const char (&str)[n]) -> string<n - 1>;

template <typename Idx, auto h>
struct string_to_type;

template <std::size_t... Idx, auto h>
struct string_to_type<std::index_sequence<Idx...>, h> {
    using type = const_string<h.str_[Idx]...>;
};
} // namespace detail

template <detail::string h>
constexpr auto operator""_() {
    using T =
        typename detail::string_to_type<std::make_index_sequence<h.str_.size()>,
                                        h>::type::integral;
    return T{};
}

#endif

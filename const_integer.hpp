#pragma once

#include "const_map.hpp"
#include <cstdint>
#include <iostream>
#include <span>
#include <string_view>
#include <utility>

template <std::size_t Value>
struct const_integer_wrapper {
    static constexpr std::size_t value = Value;

    static constexpr auto bytes = [](size_t value) {
        std::array<std::byte, sizeof(std::size_t)> res;
        for (size_t i = sizeof(std::size_t); i > 0; --i) {
            res[sizeof(size_t) - i] = (std::byte)value;
            value >>= 8;
        }
        return res;
    }(value);

    enum class integral : uint8_t {};

    friend std::ostream& operator<<(std::ostream& os, integral) {
        return os << value;
    }

    friend constexpr std::size_t to_size_t(integral) { return value; }

    template <typename T>
    friend constexpr auto operator,(integral i, T&& t) {
        return detail::const_map_getter{i, std::forward<T>(t)};
    }

    friend constexpr auto data(integral) {
        constexpr std::span s{bytes.data(), sizeof(bytes)};
        return s;
    };

    friend constexpr bool operator==(integral, std::size_t v) {
        return v == value;
    }
};

namespace detail {
constexpr std::size_t atoi(char c) {
    if (c >= '0' && c <= '9') {
        return c - '0';
    }
    if (c >= 'A' && c <= 'F') {
        return c - 'A';
    }
    if (c >= 'a' && c <= 'f') {
        return c - 'a';
    }
    throw;
}

constexpr std::size_t parse(std::string_view str, std::size_t base) {
    std::size_t result = 0;
    for (auto c : str) {
        result = base * result + atoi(c);
    }
    return result;
}

constexpr std::size_t parse(std::string_view str) {
    using namespace std::literals;
    if (str.starts_with("0b"sv)) {
        return parse({str.begin() + 2, str.end()}, 2);
    }
    if (str.starts_with("0x"sv)) {
        return parse({str.begin() + 2, str.end()}, 16);
    }
    if (str.starts_with("0"sv)) {
        return parse({str.begin() + 1, str.end()}, 8);
    }
    return parse(str, 10);
}
} // namespace detail

template <std::size_t I>
using const_integer = typename const_integer_wrapper<I>::integral;

template <char... Digits>
constexpr auto operator""_() {
    constexpr char str[] = {Digits..., '\0'};
    using T = const_integer<detail::parse(std::string_view(str))>;
    return T{};
}

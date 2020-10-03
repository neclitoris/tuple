#pragma once

#include <iostream>
#include <string_view>

template <std::size_t Value>
struct const_integer_wrapper {
    static constexpr std::size_t value = Value;

    enum class integral : uint8_t {};

    friend std::ostream& operator<<(std::ostream& os, integral) { return os << value; }

    friend constexpr std::size_t to_size_t(integral) { return value; }
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
}

template <std::size_t I>
using const_integer = typename const_integer_wrapper<I>::integral;

template <char... Digits>
constexpr auto operator""_() {
    constexpr char str[] = {Digits..., '\0'};
    using T = const_integer<detail::parse(std::string_view(str))>;
    return T{};
}

#pragma once

#include <iostream>
#include "const_string.hpp"


template<auto Value>
struct const_integer {
    static constexpr auto value = Value;

    friend std::ostream& operator<<(std::ostream& os, const_integer) {
        return os << value;
    }
};

namespace detail {
    template<char C>
    struct to_int {
        static_assert((C >= '0' && C <= '9') || (C >= 'a' && C <= 'f') || (C >= 'A' && C <= 'F'));
        static constexpr size_t value = C >= '0' && C <= '9'
                                        ? C - '0'
                                        : C >= 'a' && C <= 'f'
                                          ? C - 'a'
                                          : C - 'A';
    };

    template<char... C>
    struct pick_base {
        static constexpr size_t res = 10;
        using digits = const_string<C...>;
    };

    template<char... C>
    struct pick_base<'0', C...> {
        static constexpr size_t res = 010;
        using digits = const_string<C...>;
    };

    template<char... C>
    struct pick_base<'0', 'x', C...> {
        static constexpr size_t res = 0x10;
        using digits = const_string<C...>;
    };

    template<char... C>
    struct pick_base<'0', 'b', C...> {
        static constexpr size_t res = 0b10;
        using digits = const_string<C...>;
    };

    template<char... Digits>
    struct make_integral {
    private:
        template<auto Base, auto Acc, typename String>
        struct impl;

        template<auto Base, auto Acc, template<auto...> typename String, auto First, auto... Remaining>
        struct impl<Base, Acc, String<First, Remaining...>> {
            static constexpr auto result = impl<Base, Base * Acc + to_int<First>::value, String<Remaining...>>::result;
        };

        template<auto Base, auto Acc, template<auto...> typename String>
        struct impl<Base, Acc, String<>> {
            static constexpr auto result = Acc;
        };

    public:
        using type = const_integer<impl<pick_base<Digits...>::res, 0, typename pick_base<Digits...>::digits>::result>;
    };
}

template<char... Digits>
constexpr auto operator ""_() -> typename detail::make_integral<Digits...>::type {
    return {};
}
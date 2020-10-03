#pragma once

#include "const_integer.hpp"
#include "const_map.hpp"
#include "template_magic.hpp"
#include "tuple_ops.hpp"

#include <type_traits>
#include <utility>

namespace detail {
    template <typename T>
    struct idx_to_tuple;

    template <size_t... Idx>
    struct idx_to_tuple<std::index_sequence<Idx...>> {
        using type = type_list<const_integer<Idx>...>;
    };

    template <typename T>
    using idx_to_tuple_t = typename idx_to_tuple<T>::type;
}

template <typename... Ts>
class tuple
    : public move_to_t<const_map,
                       decltype(tuple_ops::zip(
                           std::declval<detail::idx_to_tuple_t<std::make_index_sequence<sizeof...(Ts)>>>(),
                           std::declval<type_list<Ts...>>()))> {
public:
    template <class Ch, class Tr>
    friend decltype(auto) operator<<(std::basic_ostream<Ch, Tr>& os, const tuple& t) {
        os << "(";
        print_tuple(os, t, tuple_ops::tuple_indices<tuple>{});
        return os << ")";
    }

private:
    template <class Ch, class Tr, std::size_t... Is>
    static void print_tuple(std::basic_ostream<Ch, Tr>& os, const tuple& t, std::index_sequence<Is...>) {
        (..., (os << get<Is>(t) << (Is == sizeof...(Is) - 1 ? "" : ", ")));
    }
};

template <size_t I, typename... Ts>
constexpr decltype(auto) get(tuple<Ts...>& t) {
    return t[const_integer<I>{}];
}

template <size_t I, typename... Ts>
constexpr decltype(auto) get(const tuple<Ts...>& t) {
    return t[const_integer<I>{}];
}

template <size_t I, typename... Ts>
constexpr decltype(auto) get(tuple<Ts...>&& t) {
    return std::move(t)[const_integer<I>{}];
}

template <size_t I, typename... Ts>
constexpr decltype(auto) get(const tuple<Ts...>&& t) {
    return std::move(t)[const_integer<I>{}];
}

template <typename... Ts>
tuple(Ts...) -> tuple<Ts...>;

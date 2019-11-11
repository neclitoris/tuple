#pragma once

#include "template_magic.hpp"

#include <algorithm>
#include <cstddef>
#include <type_traits>
#include <utility>

// Declare get operation; now ADL will find out
// which function should be used in template instantiations
// introduced later. Very generic!

template <size_t I, typename T>
decltype(auto) get(T&&);

namespace tuple_ops {
    template <typename T1, typename T2>
    constexpr auto operator+(T1&& t1, T2&& t2);

    template <typename F, typename T>
    constexpr auto map(F&& f, T&& t);

    template <typename T>
    constexpr auto flatten(T&& t);

    template <typename T>
    constexpr auto transpose(T&& t);

    template <typename Idx, typename T>
    constexpr auto slice(T&& t);

    template <typename... Ts>
    constexpr auto zip(Ts&&... ts);

    template <size_t N, typename Tuple>
    constexpr auto drop(Tuple&& t);

    template <size_t N, typename Tuple>
    constexpr auto chunk(Tuple&& t);

    template <class Ch, class Tr, template <class...> class Tuple, class... Args>
    decltype(auto) operator<<(std::basic_ostream<Ch, Tr>& os, Tuple<Args...> const& t);

    template <typename T>
    struct tuple_traits;

    template <template <typename...> typename Tuple, typename... Types>
    struct tuple_traits<Tuple<Types...>> {
    private:
        template <size_t I>
        static constexpr auto tuple_element_(Tuple<Types...> t) {
            return get<I>(t);
        }

    public:
        static constexpr size_t tuple_size_v = sizeof...(Types);
        template <size_t I>
        using tuple_element_t = decltype(tuple_element_<I>(std::declval<Tuple<Types...>>()));
    };

    template <typename T>
    struct tuple_size {
        static constexpr size_t value = tuple_traits<T>::tuple_size_v;
    };

    template <typename T>
    inline constexpr size_t tuple_size_v = tuple_size<T>::value;

    template <size_t I, typename T>
    struct tuple_element {
        using type = typename tuple_traits<T>::template tuple_element_t<I>;
    };

    template <size_t I, typename T>
    using tuple_element_t = typename tuple_element<I, T>::type;

    template <typename T>
    using tuple_indices = std::make_index_sequence<tuple_size_v<std::decay_t<T>>>;

    template <typename T>
    struct impls;

    template <template <typename...> typename Tuple, typename... Types>
    struct impls<Tuple<Types...>> {
        template <typename F, typename T, size_t... Idx>
        static constexpr auto map(F&& f, T&& t, std::index_sequence<Idx...>) {
            return Tuple{f(get<Idx>(std::forward<T>(t)))...};
        }

        template <typename T, size_t... Rows>
        static constexpr auto flatten(T&& t, std::index_sequence<Rows...>) {
            return Tuple{(get<Rows>(std::forward<T>(t)) + ...)};
        }

        template <typename T, size_t... Rows, size_t... Cols>
        static constexpr auto transpose(T&& t, std::index_sequence<Rows...> r,
                                        std::index_sequence<Cols...>) {
            return tuple_cat(make_tuple(
                map([](auto&& t) { return std::get<Cols>(std::forward<decltype(t)>(t)); },
                    std::forward<T>(t)))...);
        }

        template <typename T, size_t... Rows>
        static constexpr auto transpose(T&& t, std::index_sequence<Rows...> r) {
            if constexpr (tuple_size_v<T>> 0) {
                constexpr size_t len = tuple_size_v<std::tuple_element_t<0, T>>;
                constexpr bool all_lens_equal =
                    ((tuple_size_v<std::tuple_element_t<Rows, T>> == len) && ...);
                static_assert(all_lens_equal);
                return transpose(std::forward<T>(t), r, std::make_index_sequence<len>{});
            } else {
                return Tuple{};
            }
        }

        template <typename T, size_t... Idx>
        static constexpr auto slice(T&& t, std::index_sequence<Idx...>) {
            return Tuple{get<Idx>(std::forward<T>(t))...};
        }

        template <size_t I, typename T>
        static constexpr auto chunk(T&& t) {
            if constexpr (sizeof...(Types) >= I) {
                using rest = std::decay_t<decltype(drop<I>(std::declval<T>()))>;
                using head = std::decay_t<decltype(
                    tuple_ops::slice<std::make_index_sequence<I>>(std::forward<T>(t)))>;
                return tuple_cat(
                    Tuple<head>{tuple_ops::slice<std::make_index_sequence<I>>(std::forward<T>(t))},
                    impls<rest>::template chunk<I>(drop<I>(std::forward<T>(t))));
            } else {
                return t;
            }
        }
    };

    template <typename T1, typename T2>
    constexpr auto operator+(T1&& t1, T2&& t2) {
        return tuple_cat(std::forward<T1>(t1), std::forward<T2>(t2));
    }

    template <typename F, typename T>
    constexpr auto map(F&& f, T&& t) {
        return impls<std::decay_t<T>>::map(std::forward<F>(f), std::forward<T>(t),
                                           tuple_indices<T>{});
    }

    template <typename T>
    constexpr auto flatten(T&& t) {
        return impls<std::decay_t<T>>::flatten(std::forward<T&&>(t), tuple_indices<T>{});
    }

    template <typename T>
    constexpr auto transpose(T&& t) {
        return impls<std::decay_t<T>>::transpose(std::forward<T>(t), tuple_indices<T>{});
    }

    template <typename Idx, typename T>
    constexpr auto slice(T&& t) {
        return impls<std::decay_t<T>>::slice(std::forward<T>(t), Idx{});
    }

    template <typename... Ts>
    constexpr auto zip(Ts&&... ts) {
        constexpr size_t min_length = std::min({tuple_size_v<Ts>...});
        using seq = std::make_index_sequence<min_length>;
        return transpose(map([](auto&& t) { return slice<seq>(std::forward<decltype(t)>(t)); },
                             std::tuple{std::forward<Ts>(ts)...}));
    }

    template <size_t N, typename Tuple>
    constexpr auto drop(Tuple&& t) {
        return slice<make_range_t<N, tuple_size_v<std::decay_t<Tuple>>>>(t);
    }

    template <size_t N, typename Tuple>
    constexpr auto chunk(Tuple&& t) {
        return impls<std::decay_t<Tuple>>::template chunk<2>(std::forward<Tuple>(t));
    }

    template <std::size_t...>
    struct seq {};

    template <std::size_t N, std::size_t... Is>
    struct gen_seq : gen_seq<N - 1, N - 1, Is...> {};

    template <std::size_t... Is>
    struct gen_seq<0, Is...> : seq<Is...> {};

    template <class Ch, class Tr, class Tuple, std::size_t... Is>
    static void print_tuple(std::basic_ostream<Ch, Tr>& os, Tuple const& t, seq<Is...>) {
        ((os << (Is == 0 ? "" : ", ") << get<Is>(t)), ...);
    }

    template <class Ch, class Tr, template <class...> class Tuple, class... Args>
    decltype(auto) operator<<(std::basic_ostream<Ch, Tr>& os, Tuple<Args...> const& t) {
        os << "(";
        print_tuple(os, t, gen_seq<sizeof...(Args)>{});
        return os << ")";
    }
}  // namespace tuple_ops

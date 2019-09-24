#pragma once

#include "basics.hpp"
#include <algorithm>
#include <iostream>
#include <tuple>
#include <type_traits>
#include <utility>


template<typename T>
struct length;

template<template<typename...> typename Container, typename... Ts>
struct length<Container<Ts...>> {
    static constexpr size_t res = sizeof...(Ts);
};

template<typename... A>
struct index_sequence_cat {
    using res = std::index_sequence<>;
};

template<size_t... First, size_t... Second, typename... Other>
struct index_sequence_cat<std::index_sequence<First...>, std::index_sequence<Second...>, Other...> {
    using res = typename index_sequence_cat<std::index_sequence<First..., Second...>, Other...>::res;
};

template<size_t... First>
struct index_sequence_cat<std::index_sequence<First...>> {
    using res = std::index_sequence<First...>;
};

template<typename... Ids>
using index_sequence_cat_t = typename index_sequence_cat<Ids...>::res;

template<size_t From, size_t To, typename = void>
struct make_range;

template<size_t From, size_t To>
struct make_range<From, To, std::enable_if_t<From < To>> {
    using res = index_sequence_cat_t<std::index_sequence<From>, typename make_range<From + 1, To>::res>;
};

template<size_t From, size_t To>
struct make_range<From, To, std::enable_if_t<From >= To>> {
    using res = std::index_sequence<>;
};

template<size_t From, size_t To>
using make_range_t = typename make_range<From, To>::res;

template<size_t Index>
struct idexer {
public:
    template<typename Container>
    using m = std::tuple_element_t<Index, Container>;
};

template<typename T>
struct slicer;

template<size_t... Indices>
struct slicer<std::index_sequence<Indices...>> {
private:
    template<typename T>
    struct Impl;

    template<template<typename...> typename Container, typename... Ts>
    struct Impl<Container<Ts...>> {
    public:
        using res = Container<std::tuple_element_t<Indices, Container<Ts...>>...>;
    };

public:
    template<typename Container>
    using m = typename Impl<Container>::res;
};

template<size_t... Indices>
using slicer_idx = slicer<std::index_sequence<Indices...>>;

template<typename Container, typename... Values>
struct append;

template<template<typename...> typename Container, typename... Content, typename... Values>
struct append<Container<Content...>, Values...> {
    using type = Container<Content..., Values...>;
};

template<typename Container, typename... Values>
using append_t = typename append<Container, Values...>::type;

template<size_t Index, size_t Count>
using remove_index_t = index_sequence_cat_t<make_range_t<0, Index>, make_range_t<Index + 1, Count>>;

template<size_t Index, typename Container>
using remove_t = typename slicer<remove_index_t<Index, length<Container>::res>>::template m<Container>;

template<typename T, typename U>
using consume_t = U;

template<typename... Containers>
struct concat;

template<template<typename...> typename Container, typename... T1s, typename... T2s, typename... Rest>
struct concat<Container<T1s...>, Container<T2s...>, Rest...> {
    using type = typename concat<Container<T1s..., T2s...>, Rest...>::type;
};

template<typename T>
struct concat<T> {
    using type = T;
};

template<typename... Containers>
using concat_t = typename concat<Containers...>::type;

template<typename T>
struct indices_to_types;

template<size_t... Indices>
struct indices_to_types<std::index_sequence<Indices...>> {
    using type = std::tuple<std::integral_constant<size_t, Indices>...>;
};

template<typename T>
using indices_to_types_t = typename indices_to_types<T>::type;

template<typename Idx>
struct tuple_slice_impl;

template<size_t... Indices>
struct tuple_slice_impl<std::index_sequence<Indices...>> {
    template<typename Tuple>
    static constexpr auto slice(Tuple&& t) {
        return std::tuple{std::get<Indices>(std::forward<Tuple>(t))...};
    }
};

template<typename Indices, typename Tuple>
constexpr auto tuple_slice(Tuple&& t) {
    return tuple_slice_impl<Indices>::slice(std::forward<Tuple>(t));
}

template<size_t Index, typename Tuple>
auto tuple_remove(Tuple&& t) {
    return tuple_slice<remove_index_t<Index, std::tuple_size_v<Tuple>>>(std::forward<Tuple>(t));
}

template<template<typename...> typename C>
struct mover {
private:
    template<typename T>
    class impl_;

    template<template<typename...> typename C2, typename... Ts>
    class impl_<C2<Ts...>> {
        using res = C<Ts...>;
    };

public:
    template<typename T>
    using m = typename impl_<T>::res;
};

template<typename T>
struct unwrap_indices;

template<template<typename...> typename C, typename... Ts>
struct unwrap_indices<C<Ts...>> {
    using type = std::index_sequence<Ts::value...>;
};

template<typename T>
using unwrap_indices_t = typename unwrap_indices<T>::type;

template<template<typename...> typename C, template<size_t> typename W, typename T>
struct wrap_indices;

template<template<typename...> typename C, template<size_t> typename W, size_t... Indices>
struct wrap_indices<C, W, std::index_sequence<Indices...>> {
    using type = C<W<Indices>...>;
};

template<template<typename...> typename C, template<size_t> typename W, typename T>
using wrap_indices_t = typename wrap_indices<C, W, T>::type;

namespace tuple_ops {
    template<typename T>
    using tuple_indices = std::make_index_sequence<std::tuple_size_v<std::decay_t<T>>>;

    template<typename T1, typename T2, typename = std::void_t<decltype(std::tuple_cat(std::declval<T1>(), std::declval<T2>()))>>
    constexpr auto operator+(T1&& t1, T2&& t2) {
        return std::tuple_cat(std::forward<T1>(t1), std::forward<T2>(t2));
    }

    template<typename F, typename T, size_t... Idx>
    constexpr auto map_impl(F&& f, T&& t, std::index_sequence<Idx...>) {
        return std::tuple{f(std::get<Idx>(std::forward<T>(t)))...};
    }

    template<typename F, typename T>
    constexpr auto map(F&& f, T&& t) {
        return map_impl(std::forward<F>(f), std::forward<T>(t), tuple_indices<T>{});
    }

    template<typename T, size_t... Rows>
    constexpr auto flatten_impl(T&& t, std::index_sequence<Rows...>) {
        return (std::tuple{std::get<Rows>(std::forward<T>(t))} + ...);
    }

    template<typename T>
    constexpr auto flatten(T&& t) {
        return flatten_impl(std::forward<T&&>(t), tuple_indices<T>{});
    }

    template<typename T, size_t... Rows, size_t... Cols>
    constexpr auto transpose_impl(T&& t, std::index_sequence<Rows...> r, std::index_sequence<Cols...>) {
        return (std::make_tuple(map([](auto&& t) { return std::get<Cols>(std::forward<decltype(t)>(t)); },
                                                  std::forward<T>(t)
                                              )
        ) + ...);
    }

    template<typename T, size_t... Rows>
    constexpr auto transpose_impl(T&& t, std::index_sequence<Rows...> r) {
        if constexpr (std::tuple_size_v<T> > 0) {
            constexpr size_t len = std::tuple_size_v<std::tuple_element_t<0, T>>;
            constexpr bool all_lens_equal = ((std::tuple_size_v<std::tuple_element_t<Rows, T>> == len) && ...);
            static_assert(all_lens_equal);
            return transpose_impl(std::forward<T>(t), r, std::make_index_sequence<len>{});
        } else {
            return std::tuple{};
        }
    }

    template<typename T>
    constexpr auto transpose(T&& t) {
        return transpose_impl(std::forward<T>(t), tuple_indices<T>{});
    }

    template<typename... Ts>
    constexpr auto zip(Ts&& ... ts) {
        constexpr size_t min_length = std::min({std::tuple_size_v<Ts>...});
        using seq = std::make_index_sequence<min_length>;
        return transpose(map([](auto&& t) { return tuple_slice<seq>(std::forward<decltype(t)>(t)); }, std::tuple{std::forward<Ts>(ts)...}));
    }

    template<typename Pred, typename T, size_t... Idx>
    constexpr auto filter_impl(Pred&& p, T&& t, std::index_sequence<Idx...>) {
        return ((p(std::get<Idx>(std::forward<T>(t))) ? std::forward_as_tuple(std::get<Idx>(std::forward<T>(t))) : std::tuple{}) + ...);
    }

    template<typename Pred, typename T>
    constexpr auto filter(Pred&& p, T&& t) {
        return filter_impl(std::forward<Pred>(p), std::forward<T>(t), tuple_indices<T>{});
    }

    template<size_t N, typename Tuple>
    constexpr auto drop(Tuple&& t) {
        return tuple_slice<make_range_t<N, std::tuple_size_v<std::decay_t<Tuple>>>>(t);
    }
}

template<size_t I, typename T>
decltype(auto) get(T&&);

template<class Ch, class Tr, template<class...> class Tuple, class... Args>
decltype(auto) operator<<(std::basic_ostream<Ch, Tr>& os, Tuple<Args...> const& t);

template<std::size_t...>
struct seq {};

template<std::size_t N, std::size_t... Is>
struct gen_seq : gen_seq<N - 1, N - 1, Is...> {};

template<std::size_t... Is>
struct gen_seq<0, Is...> : seq<Is...> {};

template<class Ch, class Tr, class Tuple, std::size_t... Is>
void print_tuple(std::basic_ostream<Ch, Tr>& os, Tuple const& t, seq<Is...>) {
    ((os << (Is == 0 ? "" : ", ") << get<Is>(t)), ...);
}

template<class Ch, class Tr, template<class...> class Tuple, class... Args>
decltype(auto) operator<<(std::basic_ostream<Ch, Tr>& os, Tuple<Args...> const& t) {
    os << "(";
    print_tuple(os, t, gen_seq<sizeof...(Args)>{});
    return os << ")";
}

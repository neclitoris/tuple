#pragma once

#include "Basics.hpp"
#include <iostream>
#include <tuple>
#include <utility>


template<typename T>
struct Length;

template<template<typename...> typename Container, typename... Ts>
struct Length<Container<Ts...>> {
    static constexpr size_t res = sizeof...(Ts);
};

template<typename... A>
struct Index_sequence_cat {
    using res = std::index_sequence<>;
};

template<size_t... First, size_t... Second, typename... Other>
struct Index_sequence_cat<std::index_sequence<First...>, std::index_sequence<Second...>, Other...> {
    using res = typename Index_sequence_cat<std::index_sequence<First..., Second...>, Other...>::res;
};

template<size_t... First>
struct Index_sequence_cat<std::index_sequence<First...>> {
    using res = std::index_sequence<First...>;
};

template<typename... Ids>
using Index_sequence_cat_t = typename Index_sequence_cat<Ids...>::res;

template<size_t From, size_t To, typename = void>
struct Make_range;

template<size_t From, size_t To>
struct Make_range<From, To, std::enable_if_t<From < To>> {
    using res = Index_sequence_cat_t<std::index_sequence<From>, typename Make_range<From + 1, To>::res>;
};

template<size_t From, size_t To>
struct Make_range<From, To, std::enable_if_t<From >= To>> {
    using res = std::index_sequence<>;
};

template<size_t From, size_t To>
using Make_range_t = typename Make_range<From, To>::res;

template<typename T>
struct Remove_rvalue_reference {
    using res = T;
};

template<typename T>
struct Remove_rvalue_reference<T&&> {
    using res = T;
};

template<typename T>
using Remove_rvalue_reference_t = typename Remove_rvalue_reference<T>::res;

template<typename Container, template<typename> typename Mapper>
struct Map_types;

template<template<typename...> typename Container_type, typename... Ts,
        template<typename> typename Mapper>
struct Map_types<Container_type<Ts...>, Mapper> {
public:
    using res = Container_type<Mapper<Ts>...>;
};

template<typename Container, template<typename> typename Mapper>
using Map_types_t = typename Map_types<Container, Mapper>::res;

template<typename Container, auto Mapper>
struct Map_vals;

template<template<auto...> typename Container_type, auto... Ts, auto Mapper>
struct Map_vals<Container_type<Ts...>, Mapper> {
public:
    using res = Container_type<Mapper(Ts)...>;
};

template<typename Container, auto Mapper>
using Map_vals_t = typename Map_vals<Container, Mapper>::res;

template<size_t Index>
struct Indexer {
public:
    template<typename Container>
    using m = std::tuple_element_t<Index, Container>;
};

template<typename T>
struct Slicer;

template<size_t... Indices>
struct Slicer<std::index_sequence<Indices...>> {
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
using Slicer_idx = Slicer<std::index_sequence<Indices...>>;

template<typename Container, typename... Values>
struct Append;

template<template<typename...> typename Container, typename... Content, typename... Values>
struct Append<Container<Content...>, Values...> {
    using type = Container<Content..., Values...>;
};

template<typename Container, typename... Values>
using Append_t = typename Append<Container, Values...>::type;

template<size_t Index, size_t Count>
using Remove_index_t = Index_sequence_cat_t<Make_range_t<0, Index>, Make_range_t<Index + 1, Count>>;

template<size_t Index, typename Container>
using Remove_t = typename Slicer<Remove_index_t<Index, Length<Container>::res>>::template m<Container>;

template<typename T, typename U>
using Consume_t = U;

template<typename... Containers>
struct Concat;

template<template<typename...> typename Container, typename... T1s, typename... T2s, typename... Rest>
struct Concat<Container<T1s...>, Container<T2s...>, Rest...> {
    using type = typename Concat<Container<T1s..., T2s...>, Rest...>::type;
};

template<typename T>
struct Concat<T> {
    using type = T;
};

template<typename... Containers>
using Concat_t = typename Concat<Containers...>::type;

template<typename T>
struct Indices_to_types;

template<size_t... Indices>
struct Indices_to_types<std::index_sequence<Indices...>> {
    using type = std::tuple<std::integral_constant<size_t, Indices>...>;
};

template<typename T>
using Indices_to_types_t = typename Indices_to_types<T>::type;

template<typename Idx>
struct tuple_slice_impl;

template<size_t... Indices>
struct tuple_slice_impl<std::index_sequence<Indices...>> {
    template<typename Tuple>
    static auto slice(Tuple&& t) {
        return std::tuple{std::get<Indices>(std::forward<Tuple>(t))...};
    }
};

template<typename Indices, typename Tuple>
auto tuple_slice(Tuple&& t) {
    return tuple_slice_impl<Indices>::slice(std::forward<Tuple>(t));
}

template<size_t Index, typename Tuple>
auto tuple_remove(Tuple&& t) {
    return tuple_slice<Remove_index_t<Index, std::tuple_size_v<Tuple>>>(std::forward<Tuple>(t));
}

template<template<typename...> typename C>
struct Mover {
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

template<template<auto...> typename C>
struct Mover_val {
private:
    template<typename T>
    class impl_;

    template<template<auto...> typename C2, auto... Vals>
    class impl_<C2<Vals...>> {
        using res = C<Vals...>;
    };

public:
    template<typename T>
    using m = typename impl_<T>::res;
};

template<typename T>
struct Unwrap_indices;

template<template<typename...> typename C, typename... Ts>
struct Unwrap_indices<C<Ts...>> {
    using type = std::index_sequence<Ts::value...>;
};

template<typename T>
using Unwrap_indices_t = typename Unwrap_indices<T>::type;

template<template<typename...> typename C, typename T>
struct Wrap_indices;

template<template<typename...> typename C, size_t... Indices>
struct Wrap_indices<C, std::index_sequence<Indices...>> {
    using type = C<std::integral_constant<size_t, Indices>...>;
};

template<template<typename...> typename C, typename T>
using Wrap_indices_t = typename Wrap_indices<C, T>::type;

template<std::size_t...>
struct seq {};

template<std::size_t N, std::size_t... Is>
struct gen_seq : gen_seq<N - 1, N - 1, Is...> {};

template<std::size_t... Is>
struct gen_seq<0, Is...> : seq<Is...> {};

template<class Ch, class Tr, class Tuple, std::size_t... Is>
void print_tuple(std::basic_ostream<Ch, Tr>& os, Tuple const& t, seq<Is...>) {
    using swallow = int[];
    (void) swallow{0, (void(os << (Is == 0 ? "" : ", ") << std::get<Is>(t)), 0)...};
}

template<class Ch, class Tr, template<class...> class Tuple, class... Args>
auto operator<<(std::basic_ostream<Ch, Tr>& os, Tuple<Args...> const& t)
-> std::basic_ostream<Ch, Tr>& {
    os << "(";
    print_tuple(os, t, gen_seq<sizeof...(Args)>());
    return os << ")";
}
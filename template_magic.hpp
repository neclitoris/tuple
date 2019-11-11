#pragma once

#include <algorithm>
#include <iostream>
#include <tuple>
#include <type_traits>
#include <utility>
#include "basics.hpp"

template <typename T>
struct length;

template <template <typename...> typename Container, typename... Ts>
struct length<Container<Ts...>> {
    static constexpr size_t res = sizeof...(Ts);
};

template <typename... A>
struct index_sequence_cat {
    using res = std::index_sequence<>;
};

template <size_t... First, size_t... Second, typename... Other>
struct index_sequence_cat<std::index_sequence<First...>, std::index_sequence<Second...>, Other...> {
    using res =
        typename index_sequence_cat<std::index_sequence<First..., Second...>, Other...>::res;
};

template <size_t... First>
struct index_sequence_cat<std::index_sequence<First...>> {
    using res = std::index_sequence<First...>;
};

template <typename... Ids>
using index_sequence_cat_t = typename index_sequence_cat<Ids...>::res;

template <size_t From, size_t To, typename = void>
struct make_range;

template <size_t From, size_t To>
struct make_range<From, To, std::enable_if_t<(From < To)>> {
    using res =
        index_sequence_cat_t<std::index_sequence<From>, typename make_range<From + 1, To>::res>;
};

template <size_t From, size_t To>
struct make_range<From, To, std::enable_if_t<From >= To>> {
    using res = std::index_sequence<>;
};

template <size_t From, size_t To>
using make_range_t = typename make_range<From, To>::res;

template <size_t Index>
struct idexer {
public:
    template <typename Container>
    using m = std::tuple_element_t<Index, Container>;
};

template <typename Container, typename... Values>
struct append;

template <template <typename...> typename Container, typename... Content, typename... Values>
struct append<Container<Content...>, Values...> {
    using type = Container<Content..., Values...>;
};

template <typename Container, typename... Values>
using append_t = typename append<Container, Values...>::type;

template <typename T, typename U>
using consume_t = U;

template <typename... Containers>
struct concat;

template <template <typename...> typename Container, typename... T1s, typename... T2s,
          typename... Rest>
struct concat<Container<T1s...>, Container<T2s...>, Rest...> {
    using type = typename concat<Container<T1s..., T2s...>, Rest...>::type;
};

template <typename T>
struct concat<T> {
    using type = T;
};

template <typename... Containers>
using concat_t = typename concat<Containers...>::type;

template <typename T>
struct indices_to_types;

template <size_t... Indices>
struct indices_to_types<std::index_sequence<Indices...>> {
    using type = std::tuple<std::integral_constant<size_t, Indices>...>;
};

template <typename T>
using indices_to_types_t = typename indices_to_types<T>::type;

template <template <typename...> typename C>
struct mover {
private:
    template <typename T>
    struct impl_;

    template <template <typename...> typename C2, typename... Ts>
    struct impl_<C2<Ts...>> {
        using res = C<Ts...>;
    };

public:
    template <typename T>
    using m = typename impl_<T>::res;
};

template <typename T>
struct unwrap_indices;

template <template <typename...> typename C, typename... Ts>
struct unwrap_indices<C<Ts...>> {
    using type = std::index_sequence<Ts::value...>;
};

template <typename T>
using unwrap_indices_t = typename unwrap_indices<T>::type;

template <template <typename...> typename C, template <size_t> typename W, typename T>
struct wrap_indices;

template <template <typename...> typename C, template <size_t> typename W, size_t... Indices>
struct wrap_indices<C, W, std::index_sequence<Indices...>> {
    using type = C<W<Indices>...>;
};

template <template <typename...> typename C, template <size_t> typename W, typename T>
using wrap_indices_t = typename wrap_indices<C, W, T>::type;

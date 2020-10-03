#pragma once

#include "basics.hpp"
#include <algorithm>
#include <iostream>
#include <tuple>
#include <type_traits>
#include <utility>

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
    using res = typename index_sequence_cat<std::index_sequence<First..., Second...>, Other...>::res;
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
    using res = index_sequence_cat_t<std::index_sequence<From>, typename make_range<From + 1, To>::res>;
};

template <size_t From, size_t To>
struct make_range<From, To, std::enable_if_t<From >= To>> {
    using res = std::index_sequence<>;
};

template <size_t From, size_t To>
using make_range_t = typename make_range<From, To>::res;

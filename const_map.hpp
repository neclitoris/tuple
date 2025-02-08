#pragma once

#include "const_integer.hpp"
#include "perfect_hash.hpp"
#include "template_magic.hpp"
#include "type_list.hpp"

#include <iostream>
#include <type_traits>
#include <utility>
#include <variant>

template <typename... KVs>
class const_map_impl;

namespace detail {
template <typename Key, typename Value, bool = std::is_class_v<Value>>
class const_map_getter_impl;

template <typename Key, typename Value>
class const_map_getter_impl<Key, Value, true> : Value {
  public:
    constexpr const_map_getter_impl(Key, Value v) : Value(std::move(v)) {}

    constexpr Value& operator[](Key) & { return *this; }
    constexpr const Value& operator[](Key) const& { return *this; }
    constexpr Value&& operator[](Key) && { return *this; }
    constexpr const Value&& operator[](Key) const&& { return *this; }
};

template <typename Key, typename Value>
class const_map_getter_impl<Key, Value, false> {
  public:
    constexpr const_map_getter_impl(Key, Value v) : val_(std::move(v)) {}

    constexpr Value& operator[](Key) & { return val_; }
    constexpr const Value& operator[](Key) const& { return val_; }
    constexpr Value&& operator[](Key) && { return val_; }
    constexpr const Value&& operator[](Key) const&& { return val_; }

  private:
    Value val_;
};

template <typename Key, typename Value>
class const_map_getter : public const_map_getter_impl<Key, Value> {
  public:
    using key_type = Key;

  public:
    constexpr const_map_getter(Value v)
        : const_map_getter_impl<Key, Value>(Key{}, std::move(v)) {}
    constexpr const_map_getter(Key k, Value v)
        : const_map_getter_impl<Key, Value>(std::move(k), std::move(v)) {}

    template <template <typename...> typename P>
    constexpr const_map_getter(P<Key, Value>&& p)
        : const_map_getter(get<0>(decltype(p)(p)), get<1>(decltype(p)(p))) {}
};

template <typename Key, typename Value>
const_map_getter(Key, Value)
    -> const_map_getter<std::decay_t<Key>, std::decay_t<Value>>;

template <class Ch, class Tr, typename K, typename V>
decltype(auto) operator<<(std::basic_ostream<Ch, Tr>& os,
                          const const_map_getter<K, V>& g) {
    return os << K{} << " -> " << g[K{}];
}

template <typename T>
struct const_map_traits;

template <template <typename, typename> typename... Wrappers, typename... Keys,
          typename... Values>
struct const_map_traits<const_map_impl<Wrappers<Keys, Values>...>> {
    using keys = type_list<Keys...>;
    using values = type_list<Values...>;
};

template <typename T>
using const_map_keys_t = typename const_map_traits<T>::keys;

template <typename T>
using const_map_values_t = typename const_map_traits<T>::values;
} // namespace detail

template <typename... KVs>
class const_map_impl : public KVs... {
  public:
    using KVs::operator[]...;

    constexpr auto operator[](Binary auto key) {
        return (this->*visitors_.at(table_[key]))();
    }

    constexpr void visit(Binary auto key, auto callback) {
        std::visit(callback, (*this)[key]);
    }

    template <class Ch, class Tr>
    friend decltype(auto) operator<<(std::basic_ostream<Ch, Tr>& os,
                                     const const_map_impl& mp) {
        os << "{";
        print_map(os, mp, std::make_index_sequence<sizeof...(KVs)>{});
        return os << "}";
    }

    void check() {
        [this]<size_t... Idx>(std::index_sequence<Idx...>) {
            auto key = []<template <typename, typename> typename P,
                          typename Key, typename Value>(const P<Key, Value>&) {
                return Key{};
            };
            static_assert(
                ((visitors_[table_[key(static_cast<const KVs&>(*this))]] ==
                  &const_map_impl<KVs...>::template visit<KVs>) &&
                 ...),
                "This is a bug");
        }(std::make_index_sequence<sizeof...(KVs)>{});
    }

  private:
    template <class Ch, class Tr, std::size_t... Is>
    static void print_map(std::basic_ostream<Ch, Tr>& os,
                          const const_map_impl& mp,
                          std::index_sequence<Is...>) {
        using std::get;
        (...,
         (os << static_cast<KVs>(mp) << (Is == sizeof...(Is) - 1 ? "" : ", ")));
    }

    using visit_return_type =
        move_to_t<std::variant,
                  detail::const_map_values_t<const_map_impl<KVs...>>>;

    template <typename KV>
    visit_return_type visit() {
        using Key = decltype([]<template <typename, typename> typename P,
                                typename Key, typename Value>(P<Key, Value>) {
            return Key{};
        }(std::declval<KV>()));
        constexpr size_t I = lookup_v<KV, const_map_impl<KVs...>>;
        return visit_return_type{std::in_place_index<I>, (*this)[Key{}]};
    }

  private:
    static constexpr decltype([]<typename... Keys>(type_list<Keys...>) {
        return perfect_hash_table<Keys{}...>{};
    }(detail::const_map_keys_t<const_map_impl<KVs...>>{})) table_{};

    static constexpr std::array<visit_return_type (const_map_impl::*)(),
                                sizeof...(KVs)>
        visitors_ = []() {
            auto key = []<template <typename, typename> typename P,
                          typename Key, typename Value>(P<Key, Value>) {
                return Key{};
            };
            std::index_sequence<table_[decltype(key(std::declval<KVs>())){}]...>
                seq;
            std::array<visit_return_type (const_map_impl::*)(), sizeof...(KVs)>
                res;
            [&]<size_t... Is>(std::index_sequence<Is...>) {
                (..., (res[Is] = &const_map_impl<KVs...>::template visit<KVs>));
            }(seq);
            return res;
        }();
};

template <typename... KVs>
class const_map;

template <typename... KVs>
class const_map
    : public const_map_impl<move_to_t<detail::const_map_getter, KVs>...> {
  public:
    constexpr const_map(KVs&&... kvs)
        : const_map_impl<move_to_t<detail::const_map_getter, KVs>...>{
              std::forward<KVs>(kvs)...} {}
};

template <typename... KVs>
const_map(KVs...) -> const_map<KVs...>;

#pragma once

#include "type_list.hpp"

#include <iostream>
#include <utility>

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
} // namespace detail

template <typename... KVs>
class const_map_impl : public KVs... {
  public:
    using KVs::operator[]...;

    template <class Ch, class Tr>
    friend decltype(auto) operator<<(std::basic_ostream<Ch, Tr>& os,
                                     const const_map_impl& mp) {
        os << "{";
        print_map(os, mp, std::make_index_sequence<sizeof...(KVs)>{});
        return os << "}";
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
};

template <typename... KVs>
class const_map
    : public const_map_impl<move_to_t<detail::const_map_getter, KVs>...> {};

template <typename... KVs>
const_map(KVs...) -> const_map<KVs...>;

#pragma once

#include <utility>
#include "basics.hpp"
#include "template_magic.hpp"

namespace detail {
    template <typename Key, typename Value, bool = std::is_class_v<Value>>
    class typemap_getter;

    template <typename Key, typename Value>
    class typemap_getter<Key, Value, true> : Value {
       public:
        constexpr typemap_getter(Key, Value v) : Value(std::move(v)) {}

        constexpr Value& operator[](Key k) & { return static_cast<Value&>(*this); }
        constexpr const Value& operator[](Key k) const& { return static_cast<const Value&>(*this); }
        constexpr Value&& operator[](Key k) && { return static_cast<Value&&>(*this); }
        constexpr const Value&& operator[](Key k) const&& { return static_cast<Value&&>(*this); }
    };

    template <typename Key, typename Value>
    class typemap_getter<Key, Value, false> {
       public:
        constexpr typemap_getter(Key, Value v) : val_(std::move(v)) {}

        constexpr Value& operator[](Key k) & { return val_; }
        constexpr const Value& operator[](Key k) const& { return val_; }
        constexpr Value&& operator[](Key k) && { return val_; }
        constexpr const Value&& operator[](Key k) const&& { return val_; }

       private:
        Value val_;
    };

    template <typename Key, typename Value>
    typemap_getter(Key, Value)->typemap_getter<std::decay_t<Key>, std::decay_t<Value>>;
}  // namespace detail

template <typename... Getters>
class const_map_impl;

template <typename... Keys, typename... Values>
class const_map_impl<detail::typemap_getter<Keys, Values>...>
    : public detail::typemap_getter<Keys, Values>... {};

template <typename... Ts>
class tuple
    : public const_map_impl<decltype(tuple_ops::zip(
          wrap_indices_t<std::tuple, const_integer, std::make_index_sequence<sizeof...(Ts)>>{},
          std::declval<std::tuple<Ts...>>()))> {};

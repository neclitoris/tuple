#pragma once

#include "const_integer.hpp"
#include "template_magic.hpp"
#include "tuple_ops.hpp"
#include "type_list.hpp"

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
        constexpr const_map_getter(Value v) : const_map_getter_impl<Key, Value>(Key{}, std::move(v)) {}
        constexpr const_map_getter(Key k, Value v) : const_map_getter_impl<Key, Value>(std::move(k), std::move(v)) {}

        template <template <typename...> typename P>
        constexpr const_map_getter(P<Key, Value>&& p)
            : const_map_getter(get<0>(decltype(p)(p)), get<1>(decltype(p)(p))) {}
    };

    template <typename Key, typename Value>
    const_map_getter(Key, Value) -> const_map_getter<std::decay_t<Key>, std::decay_t<Value>>;
} // namespace detail

template <typename... KVs>
class const_map_impl : public KVs... {
public:
    using KVs::operator[]...;
};

template <typename... KVs>
using const_map = const_map_impl<move_to_t<detail::const_map_getter, KVs>...>;

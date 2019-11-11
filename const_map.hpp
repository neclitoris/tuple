#pragma once

#include <utility>
#include "const_integer.hpp"
#include "template_magic.hpp"
#include "tuple_ops.hpp"

namespace detail {
    template <typename Key, typename Value, bool = std::is_class_v<Value>>
    class typemap_getter_impl;

    template <typename Key, typename Value>
    class typemap_getter_impl<Key, Value, true> : Value {
    public:
        constexpr typemap_getter_impl(Key, Value v) : Value(std::move(v)) {}

        constexpr Value& operator[](const Key& k) & { return *this; }
        constexpr const Value& operator[](const Key& k) const& { return *this; }
        constexpr Value&& operator[](const Key& k) && { return *this; }
        constexpr const Value&& operator[](const Key& k) const&& { return *this; }
    };

    template <typename Key, typename Value>
    class typemap_getter_impl<Key, Value, false> {
    public:
        constexpr typemap_getter_impl(Key, Value v) : val_(std::move(v)) {}

        constexpr Value& operator[](const Key& k) & { return val_; }
        constexpr const Value& operator[](const Key& k) const& { return val_; }
        constexpr Value&& operator[](const Key& k) && { return val_; }
        constexpr const Value&& operator[](const Key& k) const&& { return val_; }

    private:
        Value val_;
    };

    template <typename Key, typename Value>
    class typemap_getter : public typemap_getter_impl<Key, Value> {
    public:
        using key_type = Key;

    public:
        constexpr typemap_getter(Key k, Value v)
            : typemap_getter_impl<Key, Value>(std::move(k), std::move(v)) {}

        template <template <typename...> typename P>
        constexpr typemap_getter(P<Key, Value>&& p)
            : typemap_getter(get<0>(decltype(p)(p)), get<1>(decltype(p)(p))) {}
    };

    template <typename Key, typename Value>
    typemap_getter(Key, Value)->typemap_getter<std::decay_t<Key>, std::decay_t<Value>>;

    template <typename T, typename U>
    struct is_comparable {
    private:
        template <typename rT, typename rU>
        static constexpr decltype(std::declval<rT>() == std::declval<rU>()) test(int) {
            return true;
        };

        template <typename rT, typename rU>
        static constexpr bool test(...) {
            return false;
        };

    public:
        static constexpr bool value = test<T, U>(42);
    };

    template <typename Key, typename T, std::enable_if_t<is_comparable<Key, T>::value, bool> = true>
    bool compare(Key&& key, T&& value) {
        return key == value;
    }

    template <
        typename Key,
        typename T,
        std::enable_if_t<!is_comparable<Key, T>::value, bool> = false>
    bool compare(Key&& key, T&& value) {
        return false;
    }
}  // namespace detail

template <typename... KVs>
class const_map_impl : public KVs... {
private:
    template <size_t I>
    using base = std::tuple_element_t<I, std::tuple<KVs...>>;
    template <size_t I>
    using key = typename std::tuple_element_t<I, std::tuple<KVs...>>::key_type;
    using indices = std::make_index_sequence<sizeof...(KVs)>;

private:
    template <size_t... I>
    constexpr void print(std::ostream& os, std::index_sequence<I...> idx) const {
        ((os << key<I>{} << " -> " << (*this)[key<I>{}] << (I == sizeof...(I) - 1 ? "" : ", ")),
         ...);
    }

    template <typename As, typename T, size_t I = 0>
    As* get_value_ptr(const T& value) {
        if constexpr (I == sizeof...(KVs)) {
            return nullptr;
        } else {
            if constexpr (std::is_constructible_v<As*, decltype(&(*this)[key<I>{}])>) {
                if (detail::compare(key<I>::value, value)) {
                    return static_cast<As*>(&(*this)[key<I>{}]);
                }
            }
            return get_value_ptr<As, T, I + 1>(value);
        }
    }

public:
    friend constexpr std::ostream& operator<<(std::ostream& os, const const_map_impl& mp) {
        os << '{';
        mp.print(os, indices{});
        os << '}';
        return os;
    }

    template <typename As, typename T>
    As* get_value(const T& value) {
        return get_value_ptr<As>(value);
    }
};

template <typename... KVs>
using const_map = const_map_impl<mover<detail::typemap_getter>::template m<KVs>...>;

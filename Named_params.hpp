#pragma once

#include "Typemap.hpp"
#include <iostream>
#include <optional>
#include <tuple>


template<typename Map>
struct Named_params;

namespace __detail {

    template<typename Map, typename Storage>
    struct Temp_storage {
        using map_type = Map;
        using storage_type = Storage;

        explicit constexpr Temp_storage(Storage&& s) : storage(std::move(s)) {}

        Storage storage;

        template<typename Key, typename Value>
        friend constexpr auto operator,(Temp_storage&& lhs, Temp_storage<Typemap<std::pair<Key, std::integral_constant<size_t, 0>>>, Value>&& rhs) {
            using New_map = typename Map::template write<Key, std::integral_constant<size_t, Length<Map>::res>>;
            using New_storage = Concat_t<Storage, Value>;
            return Temp_storage<New_map, New_storage>(std::tuple_cat(std::move(lhs.storage), std::move(rhs.storage)));
        }
    };

    template<typename Name, typename Params>
    struct Assignment_proxy {
        template<typename T>
        constexpr auto operator=(T&& value) {
            return operator()(value);
        }

        template<typename... Ts>
        constexpr auto operator()(Ts&& ... values) {
            return Temp_storage<Typemap<std::pair<Name, std::integral_constant<size_t, 0>>>, std::tuple<std::tuple<Ts...>>>(std::tuple(std::forward_as_tuple<Ts...>(values...)));
        }
    };
}

struct Named_params_forwarder {
    template<typename Name>
    constexpr auto operator[](Name param) {
        return __detail::Assignment_proxy<Name, std::decay_t<decltype(*this)>>();
    }
};

template<typename Map>
class Named_params {
public:
    using value_types = typename Map::values;
    using names = typename Map::keys;

private:
    template<typename Name>
    using find = typename Map::template find<Name>;

    template<typename Name>
    using at = std::tuple_element_t<1, find<Name>>;

private:
    template<typename T>
    struct size_impl;

    template<typename... Ts>
    struct size_impl<std::tuple<Ts...>> {
        static constexpr size_t result = (sizeof(Ts) + ...);
    };

    template<typename T>
    struct construct_values_impl;

    template<typename... Names, size_t... Indices, typename... Ts>
    struct construct_values_impl<
            __detail::Temp_storage<
                    Typemap<
                            std::pair<Names, std::integral_constant<size_t, Indices>>...
                    >,
                    std::tuple<Ts...>
            >
    > {
    private:
        using Val = __detail::Temp_storage<
                Typemap<
                        std::pair<Names, std::integral_constant<size_t, Indices>>...
                >,
                std::tuple<Ts...>
        >;

    public:
        template<typename T, typename V>
        struct placer;

        template<typename N, typename T, size_t... Idx>
        struct placer<std::pair<N, T>, std::index_sequence<Idx...>> {
            template<typename Args>
            static constexpr void place(T* ptr, Args&& args) {
                if (ptr)
                    new(ptr) T(std::forward<decltype(std::get<Idx>(args))...>(std::get<Idx>(args)...));
            }
        };

        template<typename V>
        struct placer<Not_a_type, V> {
            template<typename T, typename Args>
            static constexpr void place(T* ptr, Args&& args) {}
        };

        template<typename Temp>
        static constexpr void m(Temp&& tuple, Named_params& p) {
            (
                    placer<
                            find<Names>,
                            std::make_index_sequence<std::tuple_size_v<std::tuple_element_t<Indices, std::tuple<Ts...>>>>
                    >::place(p.get_ptr<Names>(42), std::forward<decltype(std::get<Indices>(tuple.storage))>(std::get<Indices>(tuple.storage))),
                    ...
            );
        }
    };

    template<typename Names>
    struct deconstruct_values_impl;

    template<typename... Names>
    struct deconstruct_values_impl<std::tuple<Names...>> {
        template<typename T>
        static constexpr void destroy(T* ptr) {
            ptr->~T();
        }

        static constexpr void m(Named_params& p) {
            (destroy(p.get_ptr<Names>(42)), ...);
        }
    };

    template<typename Name, typename Names>
    struct get_ptr_impl;

    template<typename Name, typename Cur, typename... Other>
    struct get_ptr_impl<Name, std::tuple<Cur, Other...>> {
        static constexpr std::optional<std::byte*> m(Named_params& p) {
            if constexpr (std::is_same_v<Name, Cur>) {
                return p.values_;
            }
            if (std::optional<std::byte*> r = get_ptr_impl<Name, std::tuple<Other...>>::m(p); r.has_value()) {
                return r.value() + sizeof(at<Cur>);
            }
            return {};
        }

        static constexpr std::optional<const std::byte*> m(const Named_params& p) {
            if constexpr (std::is_same_v<Name, Cur>) {
                return p.values_;
            }
            if (std::optional<const std::byte*> r = get_ptr_impl<Name, std::tuple<Other...>>::m(p); r.has_value()) {
                return r.value() + sizeof(at<Cur>);
            }
            return {};
        }
    };

    template<typename Name>
    struct get_ptr_impl<Name, std::tuple<>> {
        static std::optional<std::byte*> m(Named_params&) {
            return {};
        }

        static std::optional<const std::byte*> m(const Named_params&) {
            return {};
        }
    };

    template<typename T>
    struct printer;

    template<typename... Names>
    struct printer<std::tuple<Names...>> {
        static void m(std::ostream& os, const Named_params& p) {
            os << "{";
            ((os << Names{} << " = " << p[Names{}] << (std::is_same_v<std::tuple_element_t<sizeof...(Names) - 1, std::tuple<Names...>>, Names> ? "" : ", ")), ...);
            os << "}";
        }
    };

private:
    template<typename Name, std::enable_if_t<!std::is_same_v<Not_a_type, find<Name>>, int> = 0>
    constexpr auto* get_ptr(int) {
        auto res = reinterpret_cast<at<Name>*>(get_ptr_impl<Name, names>::m(*this).value_or(nullptr));
        return res;
    }

    template<typename Name>
    constexpr void* get_ptr(...) {
        return nullptr;
    }

    template<typename Name, std::enable_if_t<!std::is_same_v<Not_a_type, find<Name>>, int> = 0>
    constexpr auto* get_ptr(int) const {
        auto res = reinterpret_cast<const at<Name>*>(get_ptr_impl<Name, names>::m(*this).value_or(nullptr));
        return res;
    }

    template<typename Name>
    constexpr void* get_ptr(...) const {
        return nullptr;
    }

    template<typename T>
    constexpr void construct_values(T&& storage) {
        construct_values_impl<T>::m(std::forward<T>(storage), *this);
    }

    constexpr void deconstruct_values() {
        deconstruct_values_impl<names>::m(*this);
    }

private:
    static constexpr size_t size_ = size_impl<value_types>::result;
    std::byte values_[size_];

public:
    template<typename Temp>
    constexpr Named_params(Temp&& temp) {
        construct_values(std::forward<Temp>(temp));
    }

    ~Named_params() {
        deconstruct_values();
    }

    template<typename Name, std::enable_if_t<!std::is_same_v<Not_a_type, find<Name>>, int> = 0>
    constexpr auto operator[](const Name& name) -> at<Name>& {
        return *get_ptr<Name>(42);
    }

    template<typename Name, std::enable_if_t<!std::is_same_v<Not_a_type, find<Name>>, int> = 0>
    constexpr auto operator[](const Name& name) const -> const at<Name>& {
        return *get_ptr<Name>(42);
    }

    friend std::ostream& operator<<(std::ostream& os, const Named_params& params) {
        printer<names>::m(os, params);
        return os;
    }
};
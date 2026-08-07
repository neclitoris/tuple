#pragma once

#include <utility>

template <template <typename...> typename C, typename T>
struct move_to;

template <template <typename...> typename C, template <typename...> typename Old, typename... Ts>
struct move_to<C, Old<Ts...>> {
    using type = C<Ts...>;
};

template <template <typename...> typename C, typename T>
using move_to_t = typename move_to<C, T>::type;

template <typename... Ts>
struct type_list {
    constexpr type_list() = default;
    constexpr type_list(type_list&&) = default;
    constexpr type_list(const type_list&) = default;
    constexpr type_list& operator=(type_list&&) = default;
    constexpr type_list& operator=(const type_list&) = default;
    constexpr type_list(Ts...) {}
};

template <typename... Ts>
type_list(Ts...) -> type_list<Ts...>;

template <std::size_t I, typename T, typename... Ts>
constexpr auto get(type_list<T, Ts...>) {
    if constexpr (I == 0)
        return T{};
    else
        return get<I - 1>(type_list<Ts...>{});
}

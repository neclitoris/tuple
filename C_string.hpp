#pragma once

#include "Template_magic.hpp"
#include "Typelist.hpp"
#include <iostream>
#include <string>


template<char... Sym>
struct C_string {
private:
    template<size_t Index, char... Chs>
    struct _at {
        static constexpr char result = '\0';
    };

    template<char First, char... Chs>
    struct _at<0, First, Chs...> {
        static constexpr char result = First;
    };

    template<size_t Index, char First, char... Chs>
    struct _at<Index, First, Chs...> {
        static constexpr char result = _at<Index - 1, Chs...>::result;
    };

    template<char... Chs>
    struct _to_string {
        static void eval(std::string&) {}
    };

    template<char First, char... Chs>
    struct _to_string<First, Chs...> {
        static void eval(std::string& s) {
            s.push_back(First);
            _to_string<Chs...>::eval(s);
        }
    };

    template<typename L, typename R>
    struct _compare {
        static constexpr int result = 0;
    };

    template<char L_First, char... L_Other, char R_First, char... R_Other>
    struct _compare<C_string<L_First, L_Other...>, C_string<R_First, R_Other...>> {
        static constexpr int result = L_First - R_First ? (L_First - R_First > 0) - (L_First - R_First < 0) : _compare<C_string<L_Other...>, C_string<R_Other...>>::result;
    };

    template<char R_One, char... R_Other>
    struct _compare<C_string<>, C_string<R_One, R_Other...>> {
        static constexpr int result = -1;
    };

    template<char L_One, char... L_Other>
    struct _compare<C_string<L_One, L_Other...>, C_string<>> {
        static constexpr int result = 1;
    };

public:
    template<char S>
    using push_front = C_string<S, Sym...>;

    template<char S>
    using push_back = C_string<Sym..., S>;

    using typelist = Typelist<std::integral_constant<char, Sym>...>;

    template<size_t Index>
    static constexpr char at = _at<Index, Sym...>::result;

    static constexpr size_t count = sizeof...(Sym);
    
    static constexpr const char val[count + 1] = {Sym..., '\0'};

    static std::string to_string() {
        return val;
    }
    
    friend std::ostream& operator<<(std::ostream& os, const C_string&) {
        return os << val;
    }
    
    constexpr C_string() = default;
};

template<typename T, T... Cs>
constexpr C_string<Cs...> operator ""_() {
    static_assert(std::is_same<char, T>::value, "Only char arguments are suitable for char list literal.");
    return {};
}

template<typename...>
struct C_string_cat_impl;

template<>
struct C_string_cat_impl<> {
    using res = C_string<>;
};

template<char... Cs>
struct C_string_cat_impl<C_string<Cs...>> {
    using res = C_string<Cs...>;
};

template<char... Cs1, char... Cs2, typename... Other>
struct C_string_cat_impl<C_string<Cs1...>, C_string<Cs2...>, Other...> {
    using res = typename C_string_cat_impl<C_string<Cs1..., Cs2...>, Other...>::res;
};

template<typename... Lists>
using C_string_cat = typename C_string_cat_impl<Lists...>::res;

template<auto V, bool = (V == static_cast<decltype(V)>(0))>
struct To_c_string_impl {
    using res = C_string_cat<
            typename To_c_string_impl<V / static_cast<decltype(V)>(10)>::res,
            C_string<static_cast<char>(V % static_cast<decltype(V)>(10)) + '0'>
    >;
};

template<auto V>
struct To_c_string_impl<V, true> {
    using res = C_string<>;
};

template<auto Val>
using To_c_string = std::conditional_t<
        Val == static_cast<decltype(Val)>(0),
        C_string<'0'>,
        typename To_c_string_impl<Val>::res
>;

template<typename T, typename Ids>
struct C_string_pick_impl;

template<char... Cs, size_t... Count>
struct C_string_pick_impl<C_string<Cs...>, std::index_sequence<Count...>> {
    using res = C_string<C_string<Cs...>::template at<Count>...>;
};

template<typename T, typename U>
using C_string_pick = typename C_string_pick_impl<T, U>::res;

template<typename T, size_t Count>
using C_string_head = C_string_pick<T, Make_range_t<0, Count>>;

template<typename T, size_t Count>
using C_string_tail = C_string_pick<T, Make_range_t<T::count - Count, T::count>>;

template<typename T, size_t Count>
using C_string_no_head = C_string_tail<T, T::count - Count>;

template<typename T, size_t Count>
using C_string_no_tail = C_string_head<T, T::count - Count>;

template<typename T, typename D>
struct C_string_join_impl;

template<char... First, char... Second, typename... Other, char... Ds>
struct C_string_join_impl<Typelist<C_string<First...>, C_string<Second...>, Other...>, C_string<Ds...>> {
    using res = typename C_string_join_impl<
            Typelist<
                    C_string_cat<C_string<First...>, C_string<Ds...>, C_string<Second...>>,
                    Other...
            >,
            C_string<Ds...>
    >::res;
};

template<char... Only, char... Ds>
struct C_string_join_impl<Typelist<C_string<Only...>>, C_string<Ds...>> {
    using res = C_string<Only...>;
};

template<char... Ds>
struct C_string_join_impl<Typelist<>, C_string<Ds...>> {
    using res = C_string<>;
};

template<typename T, typename D>
using C_string_join = typename C_string_join_impl<T, D>::res;

#define C_STRING(a) decltype(#a ## _chlist)
#define C_STRING_LITERAL(a) decltype(a ## _chlist)

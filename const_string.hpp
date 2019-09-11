#pragma once

#include <iostream>
#include <string>


template<char... Sym>
struct const_string {
    static constexpr size_t count = sizeof...(Sym);
    
    static constexpr const char value[count + 1] = {Sym..., '\0'};

    explicit operator std::string() {
        return value;
    }
    
    friend std::ostream& operator<<(std::ostream& os, const const_string&) {
        return os << value;
    }
    
    template<char... OSym>
    friend constexpr bool operator==(const_string<Sym...> t1, const_string<OSym...> t2) {
        return std::is_same_v<const_string<Sym...>, const_string<OSym>...>;
    }
};

template<typename T, T... Cs>
constexpr const_string<Cs...> operator ""_() {
    static_assert(std::is_same<char, T>::value, "Only char arguments are suitable for char list literal.");
    return {};
}

#define CONST_STRING(a) decltype(#a ## _)
#define CONST_STRING_L(a) decltype(a ## _)

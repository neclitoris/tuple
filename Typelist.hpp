#ifndef MYCLASSES_TYPELIST_H
#define MYCLASSES_TYPELIST_H

#include "Basics.hpp"
#include <string>

template<typename... Types>
struct Typelist {
private:
    template<size_t Left, typename... Ts>
    struct _divide;

    template<typename... Other>
    struct _divide<0, Other...> {
    public:
        using first = Typelist<>;
        using second = Typelist<Other...>;
    };

    template<size_t Left, typename First, typename... Other>
    struct _divide<Left, First, Other...> {
    public:
        using first = typename Typelist<First>::template append<typename _divide<Left - 1, Other...>::first>::result;
        using second = typename _divide<Left - 1, Other...>::second;
    };

    template<size_t Index, typename... Chs>
    struct _at;

    template<size_t Index, typename First, typename... Chs>
    struct _at<Index, First, Chs...> {
        using res = typename _at<Index - 1, Chs...>::res;
    };

    template<typename First, typename ... Chs>
    struct _at<0, First, Chs...> {
        using res = First;
    };

public:
    static constexpr size_t count = sizeof...(Types);
    
    template<size_t Part>
    using prefix = typename _divide<Part, Types...>::first;
    
    template<size_t Part>
    using suffix = typename _divide<Part, Types...>::second;

    template<size_t Ind>
    using at = typename _at<Ind, Types...>::res;
};



#endif //MYCLASSES_TYPELIST_H

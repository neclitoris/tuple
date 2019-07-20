#pragma once

#include "Basics.hpp"
#include "Template_magic.hpp"
#include <utility>


template<typename... Pairs>
class Typemap;

template<typename... Pairs>
class Typemap {
private:
    template<typename, typename...>
    struct find_;

    template<typename Key, typename Cur_key, typename Cur_value, typename... Other>
    struct find_<Key, std::pair<Cur_key, Cur_value>, Other...> {
    private:
        template<typename T>
        static auto result_(int) -> std::enable_if_t<std::is_same_v<T, Cur_key>, std::pair<Cur_key, Cur_value>>;

        template<typename T>
        static auto result_(...) -> typename find_<Key, Other...>::result;

    public:
        using result = decltype(result_<Key>(42));
    };

    template<typename Key>
    struct find_<Key> {
        using result = Not_a_type;
    };
    
    template<typename, typename, typename...>
    struct write_;
    
    template<typename Key, typename Value, typename Cur_key, typename Cur_value, typename... Other>
    struct write_<Key, Value, std::pair<Cur_key, Cur_value>, Other...> {
    private:
        template<typename T>
        static auto result_(int) -> std::enable_if_t<std::is_same_v<T, Cur_key>, Typemap<std::pair<T, Value>, Other...>>;
        
        template<typename T>
        static auto result_(...) -> Concat_t<Typemap<std::pair<Cur_key, Cur_value>>, typename write_<Key, Value, Other...>::result>;
        
    public:
        using result = decltype(result_<Key>(42));
    };

    template<typename Key, typename Value>
    struct write_<Key, Value> {
    public:
        using result = Typemap<std::pair<Key, Value>>;
    };

public:
    template<typename K>
    using find = typename find_<K, Pairs...>::result;

    template<typename K, typename V>
    using write = typename write_<K, V, Pairs...>::result;
    
    using keys = Map_types_t<std::tuple<Pairs...>, Indexer<0>::m>;

    using values = Map_types_t<std::tuple<Pairs...>, Indexer<1>::m>;
};
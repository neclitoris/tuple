#ifndef MYCLASSES_BASICS_H
#define MYCLASSES_BASICS_H

struct Not_a_type final {};

template<typename...>
struct to_void final { typedef void type; };

#define SFINAE_REQUIRED_OPS(args...) typename to_void<decltype(args)>::type
#define SFINAE_REQUIRED_TYPES(args...) typename to_void<args>::type
#define SFINAE_REQUIRED(args...) typename to_void<args>::type
#define SFINAE_SLOT typename = void
#define SFINAE_OK void

#endif //MYCLASSES_BASICS_H

#ifndef MYCLASSES_BASICS_H
#define MYCLASSES_BASICS_H

struct not_a_type final {};

template<typename...>
struct to_void final { typedef void type; };

#endif //MYCLASSES_BASICS_H

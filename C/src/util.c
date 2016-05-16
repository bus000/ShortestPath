#include "util.h"

inline uint32_t min(uint32_t a, uint32_t b)
{
    return a < b ? a : b;
}

inline uint32_t max(uint32_t a, uint32_t b)
{
    return a > b ? a : b;
}

uint32_t count_char(char const *str, char c)
{
    uint32_t count = 0;

    for (count = 0; *str != '\0'; str++)
        if (*str == c)
            count += 1;

    return count;
}

inline int even(uint32_t i)
{
    return !(i % 2);
}

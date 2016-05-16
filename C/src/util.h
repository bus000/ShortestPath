#ifndef UTIL_H
#define UTIL_H

#include <inttypes.h>

/* Returns the minimum of the two numbers given. */
uint32_t min(uint32_t a, uint32_t b);

/* Returns the maximum of the two numbers given. */
uint32_t max(uint32_t a, uint32_t b);

/* Count the number of occurrences of the character c in the string str. */
uint32_t count_char(char const *str, char c);

/* Returns true if an integer i is even, false otherwise. */
int even(uint32_t i);

#endif

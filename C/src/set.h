#ifndef SET_H
#define SET_H

#include "map.h"

typedef struct {
    map_t map;
} set_t;

/* Initialize a new set. A set is underlying a hashmap, so functions needed to
 * construct that, should be given to construct the set. */
set_t set_init(int (*hash)(void const *),
        int (*cmp_keys)(void const *, void const *));

/* Add a new value to the set, the value is only added if it is not already in
 * the set. */
void set_add(set_t *set, void const *element);

/* Free the resources used by a set. */
void set_free(set_t *set);

#endif

#include "set.h"

set_t set_init(int (*hash)(void const *),
        int (*cmp_keys)(void const *, void const *))
{
    set_t set;

    map_init(&set.map, 128, hash, cmp_keys);

    return set;
}

void set_add(set_t *set, void const *element)
{
    if (!map_contains(&set->map, element))
        map_put(&set->map, element, element);
}

void set_free(set_t *set)
{
    map_free(&set->map);
}

#include "../src/map.h"
#include <stdio.h>
#include <stdlib.h>

static int hash(void const *el);
static int cmp(void const *el1, void const *el2);

int main(int argc, char const *argv[])
{
    map_t map;
    uint32_t ints[100] = { 0 }, i;

    map_init(&map, 16, hash, cmp);
    for (i = 0; i < 100; i++)
        ints[i] = i;

    if (map_contains(&map, &ints[10]))
        printf("map contains %u with the value %u\n", ints[10],
                *((uint32_t *) map_get(&map, &ints[10])));
    map_put(&map, &ints[10], &ints[20]);
    if (map_contains(&map, &ints[10]))
        printf("map contains %u with the value %u\n", ints[10],
                *((uint32_t *) map_get(&map, &ints[10])));

    if (map_contains(&map, &ints[20]))
        printf("map contains %u with the value %u\n", ints[20],
                *((uint32_t *) map_get(&map, &ints[20])));
    map_put(&map, &ints[20], &ints[10]);
    if (map_contains(&map, &ints[20]))
        printf("map contains %u with the value %u\n", ints[20],
                *((uint32_t *) map_get(&map, &ints[20])));

    for (i = 0; i < 100; i++)
        map_put(&map, &ints[i], NULL);

    for (i = 0; i < 100; i++)
        if (!map_contains(&map, &i))
            printf("map does not contain %u\n", i);

    map_free(&map);

    return EXIT_SUCCESS;
}

static int hash(void const *el)
{
    uint32_t const *i = (uint32_t const *) el;

    return *i;
}

static int cmp(void const *el1, void const *el2)
{
    uint32_t const *i = (uint32_t const *) el1;
    uint32_t const *j = (uint32_t const *) el2;

    return *i - *j;
}

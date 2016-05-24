#include "map.h"
#include "error.h"
#include "mem_man.h"
#include <stdlib.h> /* malloc. */
#include <math.h> /* HUGE_VAL. */

static inline int hash(map_t const *map, void const *key)
{
    return map->hash(key) % map->length;
}

static bucket_list_t * find_key(bucket_list_t *start, void const *key,
        int (*cmp_keys)(void const *, void const *))
{
    while (start != NULL) {
        if (cmp_keys(key, start->key) == 0)
            return start;

        start = start->next;
    }

    return NULL;
}

int map_init(map_t *map, int buckets, int (*hash)(void const *),
        int (*cmp_keys)(void const *, void const *))
{
    int i;

    map->length = buckets;
    map->hash = hash;
    map->cmp_keys = cmp_keys;

    MALLOC(map->buckets, sizeof(bucket_t) * buckets);

    for (i = 0; i < buckets; i++) {
        map->buckets[i].last = NULL;
        map->buckets[i].first = NULL;
    }

    map->keys = linked_list_init();
    map->values = linked_list_init();

    return 0;
}

void const * map_get(map_t const *map, void const *key)
{
    bucket_t bucket = map->buckets[hash(map, key)];
    bucket_list_t *entry = find_key(bucket.first, key, map->cmp_keys);

    return entry == NULL ? NULL : entry->value;
}

int map_put(map_t *map, void const *key, void const *value)
{
    bucket_t *bucket = &map->buckets[hash(map, key)];
    bucket_list_t *el;

    if (bucket->first == NULL) {
        MALLOC(bucket->first, sizeof(bucket_list_t));

        bucket->last = bucket->first;
        bucket->first->key = key;
        bucket->first->value = value;
        bucket->first->next = NULL;

        linked_list_add_end(&map->keys, key);
        linked_list_add_end(&map->values, value);

        return 0;
    } else {
        el = find_key(bucket->first, key, map->cmp_keys);
        if (el == NULL) {
            MALLOC(bucket->last->next, sizeof(bucket_list_t));

            bucket->last = bucket->last->next;
            bucket->last->key = key;
            bucket->last->value = value;
            bucket->last->next = NULL;

            linked_list_add_end(&map->keys, key);
            linked_list_add_end(&map->values, value);

            return 0;
        } else {
            return -1;
        }
    }
}

int map_contains(map_t const *map, void const *key)
{
    bucket_t bucket = map->buckets[hash(map, key)];

    return find_key(bucket.first, key, map->cmp_keys) == NULL ? 0 : 1;
}

linked_list_t map_get_keys(map_t const *map)
{
    return map->keys;
}

linked_list_t map_get_values(map_t const *map)
{
    return map->values;
}

void map_free(map_t *map)
{
    int i;
    bucket_list_t *next;
    bucket_list_t *tmp;

    for (i = 0; i < map->length; i++) {
        next = map->buckets[i].first;

        while (next != NULL) {
            tmp = next;
            next = next->next;
            FREE(tmp);
        }
    }

    linked_list_free(&map->keys);
    linked_list_free(&map->values);

    FREE(map->buckets);
}

#include "map.h"
#include "error.h"
#include <stdlib.h> /* malloc. */
#include <math.h> /* HUGE_VAL. */

static bucket_list_t * find_key(bucket_list_t *start, void const *key,
        int (*cmp_keys)(void const *, void const *));
static int hash(map_t const *map, void const *key);
static void bucket_list_print(bucket_list_t *start,
        void (*print_el)(void const *, FILE *), FILE *f);

int map_init(map_t *map, int buckets, int (*hash)(void const *),
        int (*cmp_keys)(void const *, void const *))
{
    int i;

    map->length = buckets;
    map->hash = hash;
    map->cmp_keys = cmp_keys;

    map->buckets = malloc(sizeof(bucket_t) * buckets);

    if (map->buckets == NULL)
        mem_err();

    for (i = 0; i < buckets; i++) {
        map->buckets[i].last = NULL;
        map->buckets[i].first = NULL;
    }

    return 0;
}

void const * map_get(map_t const *map, void const *key)
{
    bucket_t bucket = map->buckets[hash(map, key)];
    bucket_list_t *element = find_key(bucket.first, key, map->cmp_keys);

    return (element == NULL) ? NULL : element->value;
}

int map_put(map_t *map, void const *key, void const *value)
{
    bucket_t *bucket = map->buckets + hash(map, key);
    bucket_list_t *el;

    if (bucket->first == NULL) {
        bucket->first = malloc(sizeof(bucket_list_t));
        if (bucket->first == NULL)
            mem_err();

        bucket->last = bucket->first;
        bucket->first->key = key;
        bucket->first->value = value;
        bucket->first->next = NULL;

        return 0;
    } else {
        el = find_key(bucket->first, key, map->cmp_keys);
        if (el == NULL) {
            bucket->last->next = malloc(sizeof(bucket_list_t));
            if (bucket->last->next == NULL)
                mem_err();

            bucket->last = bucket->last->next;
            bucket->last->key = key;
            bucket->last->value = value;
            bucket->last->next = NULL;

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

void const * map_remove(map_t *map, void const *key)
{
    bucket_t *bucket = &(map->buckets[hash(map, key)]);
    bucket_list_t *prev = NULL;
    bucket_list_t *cur;
    void const *el;

    for (cur = bucket->first; cur != NULL; prev = cur, cur = cur->next) {
        if (map->cmp_keys(key, cur->key) == 0) {
            prev == NULL ? (bucket->first = cur->next) :
                (prev->next = cur->next);
            el = cur->value;
            free(cur);

            return el;
        }
    }

    return NULL;
}

void map_print(map_t const *map, void (*print_el)(void const *, FILE *),
        FILE *f)
{
    int i;
    bucket_t bucket;

    for (i = 0; i < map->length; i++) {
        bucket = map->buckets[i];
        fprintf(f, "{");
        bucket_list_print(bucket.first, print_el, f);
        fprintf(f, "}\n");
    }
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
            free(tmp);
        }
    }

    free(map->buckets);
}

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

static void bucket_list_print(bucket_list_t *start,
        void (*print_el)(void const *, FILE *), FILE *f)
{
    if (start == NULL)
        return;

    print_el(start->value, f);
    start = start->next;
    for (; start != NULL; start = start->next) {
        fprintf(f, ", ");
        print_el(start->value, f);
    }
}

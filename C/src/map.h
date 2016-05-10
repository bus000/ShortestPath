#ifndef MAP_H
#define MAP_H

#include "error.h"
#include "linked_list.h"
#include <stdio.h>

typedef struct bucket_list_s {
    struct bucket_list_s *next;
    void const *key;
    void const *value;
} bucket_list_t;

typedef struct {
    bucket_list_t *last;
    bucket_list_t *first;
} bucket_t;

typedef struct {
    /* Pointer to buckets containing data. */
    bucket_t *buckets;

    /* The length of the array of buckets. */
    int length;

    /* Function computing the hash of a key. */
    int (*hash)(void const *);

    /* Compare two keys. */
    int (*cmp_keys)(void const *, void const *);

    linked_list_t keys;
    linked_list_t values;
} map_t;

/* Initialize a hashmap. */
int map_init(map_t *map, int buckets, int (*hash)(void const *),
        int (*cmp_keys)(void const *, void const *));

/* Get a value from the map, return NULL if it is non existent. */
void const * map_get(map_t const *map, void const *key);

/* Insert a value into a map with the location key. Return error if memory
 * cannot be allocated or a value already exist for a key in the map. */
int map_put(map_t *map, void const *key, void const *value);

/* Query if the map contains a value for the key given. */
int map_contains(map_t const *map, void const *key);

/* Returns a list of all the keys in the map, the list should not be altered as
 * it is internal to the map. */
linked_list_t map_get_keys(map_t const *map);

linked_list_t map_get_values(map_t const *map);

/* Free resources used by a map. */
void map_free(map_t *map);

#endif

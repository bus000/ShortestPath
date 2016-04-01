#ifndef MAP_H
#define MAP_H

#include "error.h"
#include <stdio.h>

typedef struct bucket_list_ {
    struct bucket_list_ *next;
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

/* Remove value from map corresponding to the key given. Return pointer to
 * removed value on success and NULL if the element is not found. */
void const * map_remove(map_t *map, void const *key);

/* Print a map and all its elements. */
void map_print(map_t const *map, void (*print_el)(void const *, FILE *),
        FILE *f);

/* Free resources used by a map. */
void map_free(map_t *map);

#endif

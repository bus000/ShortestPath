#ifndef HEAP_H
#define HEAP_H

#include <inttypes.h>
#include <stdio.h>

#define PARENT(i) ((i-1) >> 1)
#define LEFT(i)   ((i << 1) + 1)
#define RIGHT(i)  ((2 << i) + 2)

typedef struct {
    /* The array holding the pointers. */
    uint32_t array_size;
    void **array;

    /* Function comparing two values in the heap, should return positive if the
     * first argument is greater than the second, negative if the second
     * argument is greater than the first and 0 otherwise. */
    int (*compare)(void const *el1, void const *el2);

    /* Function that takes an element of the heap, and sets the key of that
     * element as the new key given as a void pointer to the function. */
    void (*decrease_key)(void *el, void *newkey);
} min_heap_t;

int heap_init(min_heap_t *heap, void **array, uint32_t array_size,
        int (*compare)(void const *el1, void const *el2),
        void (*decrease_key)(void *el, void *newkey));

int heap_min_heapify(min_heap_t *heap, uint32_t index);

void * heap_peek_min(min_heap_t const *heap);

void * heap_extract_min(min_heap_t *heap);

int heap_decrease_key(min_heap_t *heap, uint32_t index, void *newkey);

/* Locate the pointer el in heap and decrease the key of that pointer to the
 * key given. Returns 0 on success and -1 if the element does not exist. */
int heap_decrease_element(min_heap_t *heap, void *el, void *newkey);

void heap_print(void *h, FILE *f, void (*print_el)(void *, FILE *));

#endif

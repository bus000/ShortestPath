#ifndef HEAP_H
#define HEAP_H

#include <inttypes.h>
#include <stdio.h>

#define PARENT(i) ((((int64_t) i)-1) / 2)
#define LEFT(i)   ((i * 2) + 1)
#define RIGHT(i)  ((i * 2) + 2)

typedef struct min_heap_s {
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

/* Initialize a new heap in the pointer heap, with the initial elements in the
 * array, array with the size array_size. The function compare, should take two
 * elements from the array and return -1 if the first element is less than the
 * second, 0 if they are equal and 1 if the first element is greater than the
 * second. */
int heap_init(min_heap_t *heap, void **array, uint32_t array_size,
        int (*compare)(void const *el1, void const *el2),
        void (*decrease_key)(void *el, void *newkey));

min_heap_t heap_cheap_init(void **array, uint32_t array_size,
        int (*compare)(void const *el1, void const *el2),
        void (*decrease_key)(void *el, void *newkey),
        void const *first);

/* Finds the smallest element in the heap, but keeps it in the heap. */
void * heap_peek_min(min_heap_t const *heap);

/* Find and remove the smallest element in the heap. */
void * heap_extract_min(min_heap_t *heap);

/* Moves the element with the index given higher in the heap by increasing its
 * decreasing its key. */
int heap_decrease_key(min_heap_t *heap, uint32_t index, void *newkey);

/* Locate the pointer el in heap and decrease the key of that pointer to the
 * key given. Returns 0 on success and -1 if the element does not exist. */
int heap_decrease_element(min_heap_t *heap, void *el, void *newkey);

/* Prints a heap to the file f by providing a printing function for the elements
 * in the heap. */
void heap_print(void *h, FILE *f, void (*print_el)(void *, FILE *));

/* Return true if heap is empty and false otherwise. */
int heap_empty(min_heap_t const *heap);

#endif

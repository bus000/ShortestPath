#include "heap.h"
#include <stdlib.h>

static inline void exchange(void **array, uint32_t i1, uint32_t i2)
{
    void *a = array[i1];

    array[i1] = array[i2];
    array[i2] = a;
}

static int heap_min_heapify(min_heap_t *heap, uint32_t index)
{
    uint32_t l_index = LEFT(index);
    uint32_t r_index = RIGHT(index);
    int (*compare)(void const *el1, void const *el2) = heap->compare;
    void **array = heap->array;
    uint32_t smallest;
    uint32_t array_size = heap->array_size;

    if (l_index < array_size && compare(array[l_index], array[index]) < 0)
        smallest = l_index;
    else
        smallest = index;

    if (r_index < array_size && compare(array[r_index], array[smallest]) < 0)
        smallest = r_index;

    if (smallest != index) {
        exchange(array, index, smallest);
        heap_min_heapify(heap, smallest);
    }

    return 0;
}

static void build_min_heap(min_heap_t *heap)
{
    int64_t i;

    for (i = heap->array_size / 2; i >= 0; i--)
        heap_min_heapify(heap, i);
}

int heap_init(min_heap_t *heap, void **array, uint32_t array_size,
        int (*compare)(void const *el1, void const *el2),
        void (*decrease_key)(void *el, void *newkey))
{
    heap->array = array;
    heap->array_size = array_size;
    heap->compare = compare;
    heap->decrease_key = decrease_key;

    build_min_heap(heap);

    return 0;
}

min_heap_t heap_cheap_init(void **array, uint32_t array_size,
        int (*compare)(void const *el1, void const *el2),
        void (*decrease_key)(void *el, void *newkey),
        void const *first)
{
    uint32_t i;
    void const *current;
    min_heap_t heap;

    heap.array = array;
    heap.array_size = array_size;
    heap.compare = compare;
    heap.decrease_key = decrease_key;

    for (i = 0; i < array_size; i++) {
        current = array[i];

        if (first == current)
            exchange(array, 0, i);
    }

    return heap;
}

inline void * heap_peek_min(min_heap_t const *heap)
{
    return heap->array[0];
}

void * heap_extract_min(min_heap_t *heap)
{
    void *min;

    if (heap->array_size < 1)
        return NULL;

    min = heap->array[0];
    heap->array_size -= 1;
    heap->array[0] = heap->array[heap->array_size];

    heap_min_heapify(heap, 0);

    return min;
}

int heap_decrease_key(min_heap_t *heap, uint32_t index, void *newkey)
{
    int (*compare)(void const *el1, void const *el2) = heap->compare;
    void (*decrease_key)(void *el, void *newkey) = heap->decrease_key;
    void **array = heap->array;
    uint32_t parent = PARENT(index);

    decrease_key(array[index], newkey);

    while (index > 0 && compare(array[parent], array[index]) > 0) {
        exchange(array, index, parent);
        index = parent;
        parent = PARENT(parent);
    }

    return 0;
}

int heap_decrease_element(min_heap_t *heap, void *el, void *newkey)
{
    uint32_t i;

    for (i = 0; i < heap->array_size; i++)
        if (heap->array[i] == el)
            return heap_decrease_key(heap, i, newkey);

    return -1;
}

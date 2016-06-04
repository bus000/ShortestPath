#include "heap.h"
#include <stdlib.h>

static inline void exchange(void **array, uint32_t i1, uint32_t i2,
        void (*set_index)(void *el, uint32_t index))
{
    void *p1 = array[i1];
    void *p2 = array[i2];

    if (set_index != NULL) {
        set_index(p1, i2);
        set_index(p2, i1);
    }

    array[i1] = p2;
    array[i2] = p1;
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
        exchange(array, index, smallest, heap->set_index);
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
    heap->set_index = NULL;

    build_min_heap(heap);

    return 0;
}

min_heap_t heap_cheap_init(void **array, uint32_t array_size,
        int (*compare)(void const *el1, void const *el2),
        void (*decrease_key)(void *el, void *newkey),
        void const *first, void (*set_index)(void *el, uint32_t index))
{
    uint32_t i;
    void *current;
    min_heap_t heap;

    heap.array = array;
    heap.array_size = array_size;
    heap.compare = compare;
    heap.decrease_key = decrease_key;
    heap.set_index = set_index;

    for (i = 0; i < array_size; i++) {
        current = array[i];

        if (heap.set_index != NULL)
            heap.set_index(current, i);

        if (first == current)
            exchange(array, 0, i, heap.set_index);
    }

    return heap;
}

void inline heap_set_index(min_heap_t *heap,
        void (*set_index)(void *el, uint32_t index))
{
    heap->set_index = set_index;
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
        exchange(array, index, parent, heap->set_index);
        index = parent;
        parent = PARENT(parent);
    }

    return 0;
}

int heap_empty(min_heap_t const *heap)
{
    return heap->array_size == 0;
}

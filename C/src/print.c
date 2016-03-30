#include "print.h"
#include "graph.h"
#include "heap.h"

void heap_print(void *h, FILE *f, void (*print_el)(void *, FILE *))
{
    min_heap_t *heap = (min_heap_t *) h;
    void *el;
    void **array = heap->array;
    uint32_t i;

    fprintf(f, "[");
    for (i = 0; i < heap->array_size-1; i++) {
        el = array[i];
        print_el(el, f);
        fprintf(f, ",\n");
    }
    el = array[i];
    print_el(el, f);
    fprintf(f, "]\n");
}

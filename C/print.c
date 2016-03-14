#include "print.h"
#include "graph.h"
#include "heap.h"

void vertex_print(void *v, FILE *f)
{
    vertex_t *vertex = (vertex_t *) v;

    if (vertex->print_label == NULL) {
        fprintf(f, "%p = { .unique_id = %u, .edges_len = %u, .edges_size = %u, "
                ".edges = %p, .label = %p, .free_label = %p }",
                vertex,
                vertex->unique_id,
                vertex->edges_len,
                vertex->edges_size,
                vertex->edges,
                vertex->label,
                vertex->free_label);
    } else {
        fprintf(f, "%p = { .unique_id = %u, .edges_len = %u, .edges_size = %u, "
                ".edges = %p, .label = ",
                vertex,
                vertex->unique_id,
                vertex->edges_len,
                vertex->edges_size,
                vertex->edges);
        vertex->print_label(vertex->label, f);
        fprintf(f, ", .free_label = %p }", vertex->free_label);
    }
}

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

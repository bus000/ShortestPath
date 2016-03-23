#include <stdlib.h>
#include "graph.h"
#include "error.h"

int vertex_list_init(vertex_list_t *list)
{
    list->vertices = malloc(sizeof(vertex_t *) * INIT_LIST_SIZE);
    list->len = 0;
    list->size = INIT_LIST_SIZE;

    if (list->vertices == NULL)
        mem_err();

    return 0;
}

int vertex_list_contains(vertex_list_t *list, vertex_id_t v)
{
    uint32_t i;
    vertex_t *vertex;

    for (i = 0; i < list->len; i++) {
        vertex = list->vertices[i];
        if (vertex->unique_id == v)
            return 1;
    }

    return 0;
}

void vertex_list_free(vertex_list_t *list)
{
    if (list->size != 0)
        free(list->vertices);
}

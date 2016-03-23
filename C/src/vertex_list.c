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

void vertex_list_free(vertex_list_t *list)
{
    if (list->size != 0)
        free(list->vertices);
}

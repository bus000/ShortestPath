#include <stdlib.h>
#include "graph.h"
#include "error.h"
#include "vertex_list.h"
#include "mem_man.h"
#include <string.h>

int vertex_list_init(vertex_list_t *list)
{
    MALLOC(list->vertices, sizeof(vertex_t *) * INIT_LIST_SIZE);
    list->len = 0;
    list->size = INIT_LIST_SIZE;

    return 0;
}

int vertex_list_init_array(vertex_list_t *list, vertex_t **vertices,
        size_t len)
{
    MALLOC(list->vertices, sizeof(vertex_t *) * len);

    list->len = len;
    list->size = len;
    memcpy(list->vertices, vertices, sizeof(vertex_t *) * len);

    return 0;
}

int vertex_list_init_singular(vertex_list_t *list, vertex_t *vertex)
{
    vertex_t *vertices[1] = { vertex };

    return vertex_list_init_array(list, vertices, 1);
}

int vertex_list_contains(vertex_list_t const *list, vertex_id_t v)
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
    if (list->size != 0) {
        FREE(list->vertices);
        list->size = 0;
    }
}

int vertex_list_add(vertex_list_t *list, vertex_t *vertex)
{
    size_t newsize;

    if (list->len == list->size) {
        list->size *= 2;
        newsize = sizeof(vertex_t *) * list->size;

        REALLOC(list->vertices, newsize);
    }

    list->vertices[list->len] = vertex;
    list->len += 1;

    return 0;
}

void vertex_list_empty(vertex_list_t *list)
{
    list->len = 0;
}

uint32_t vertex_list_get_index_of(vertex_list_t const *list, vertex_id_t v)
{
    uint32_t i;
    vertex_t *vertex;

    for (i = 0; i < list->len; i++) {
        vertex = list->vertices[i];
        if (vertex->unique_id == v)
            return i;
    }

    return -1;
}

uint32_t vertex_list_filter(vertex_list_t *list,
        int (*f)(vertex_t const *vertex))
{
    uint32_t i, move = 0;
    vertex_t *vertex;

    for (i = 0; i < list->len; i++) {
        vertex = list->vertices[i];

        if (!f(vertex))
            list->vertices[i] = NULL;
    }

    /* Move all vertices in graph to start of list. */
    for (i = 0; i < list->len; i++) {
        vertex = list->vertices[i];
        if (vertex == NULL) {
            move += 1;
        } else {
            list->vertices[i-move] = vertex;
        }
    }
    list->len -= move;

    return move;
}

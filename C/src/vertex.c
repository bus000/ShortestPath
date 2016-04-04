#include "vertex.h"
#include "error.h"
#include "vertex_list.h"
#include <stdlib.h>

vertex_t *vertices;
uint32_t vertices_len;
uint32_t vertices_size;

/* Returns a different unique ID each time the function is called. */
static vertex_id_t get_unique_id();

void vertices_init(void)
{
    vertices = malloc(sizeof(vertex_t) * INIT_VERTICES_SIZE);
    vertices_len = 0;
    vertices_size = INIT_VERTICES_SIZE;

    if (vertices == NULL)
        mem_err();
}

vertex_t * new_vertex(void)
{
    return new_vertex_id(get_unique_id());
}

vertex_t * new_vertex_id(vertex_id_t id)
{
    vertex_t *vertex;

    if (vertices_len >= vertices_size) {
        vertices_size *= 2;
        vertices = realloc(vertices, vertices_size);

        if (vertices == NULL)
            mem_err();
    }

    vertex = &(vertices[vertices_len]);

    vertex->unique_id = id;
    vertex->edges_len = 0;
    vertex->edges_size = INIT_EDGES_NUM;
    vertex->edges = malloc(sizeof(edge_t) * INIT_EDGES_NUM);
    vertex->label = NULL;
    vertex->visited = 0;

    if (vertex->edges == NULL)
        mem_err();

    vertices_len += 1;

    return vertex;
}

static vertex_id_t get_unique_id()
{
    static vertex_id_t unique_id = 0;

    unique_id += 1;

    return unique_id;
}

void vertex_free(vertex_t *vertex)
{
    if (vertex->edges_size != 0)
        free(vertex->edges);
}

void vertices_free(void)
{
    uint32_t i;

    for (i = 0; i < vertices_len; i++) {
        vertex_free(&vertices[i]);
    }

    free(vertices);
}

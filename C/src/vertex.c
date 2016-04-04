#include "vertex.h"
#include "error.h"
#include "vertex_list.h"
#include <stdlib.h>
#include <unistd.h>

static vertex_t **vertex_pages; /* Array of arrays of vertices. */
static uint32_t pages_size;
static long vertices_on_page;
static long next_vertex;

/* Returns a different unique ID each time the function is called. */
static vertex_id_t get_unique_id();

void vertices_init(void)
{
    vertices_on_page = sysconf(_SC_PAGESIZE) / sizeof(vertex_t);
    pages_size = 1;
    next_vertex = 0;

    if ((vertex_pages = malloc(sizeof(vertex_t *))) == NULL)
        mem_err();
    vertex_pages[0] = malloc(sizeof(vertex_t) * vertices_on_page);

    if (vertex_pages[0] == NULL)
        mem_err();
}

vertex_t * new_vertex(void)
{
    return new_vertex_id(get_unique_id());
}

vertex_t * new_vertex_id(vertex_id_t id)
{
    vertex_t *vertex;

    if (next_vertex >= vertices_on_page) {
        vertex_pages = realloc(vertex_pages,
                sizeof(vertex_t *) * (pages_size + 1));

        if (vertex_pages == NULL)
            mem_err();

        vertex_pages[pages_size] = malloc(sizeof(vertex_t) * vertices_on_page);

        if (vertex_pages[pages_size] == NULL)
            mem_err();

        pages_size += 1;
        next_vertex = 0;
    }

    vertex = &(vertex_pages[pages_size-1][next_vertex]);
    next_vertex += 1;

    vertex->unique_id = id;
    vertex->edges_len = 0;
    vertex->edges_size = INIT_EDGES_NUM;
    vertex->edges = malloc(sizeof(edge_t) * INIT_EDGES_NUM);
    vertex->label = NULL;
    vertex->visited = 0;

    if (vertex->edges == NULL)
        mem_err();

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
    long i, j, vertices_len;
    vertex_t *list, *vertex;

    for (i = 0; i < pages_size; i++) {
        list = vertex_pages[i];
        vertices_len = i < pages_size -1 ? vertices_on_page : next_vertex;
        for (j = 0; j < vertices_len; j++) {
            vertex = &(list[j]);
            vertex_free(vertex);
        }
        free(list);
    }

    free(vertex_pages);
}

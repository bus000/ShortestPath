#include "vertex.h"
#include "mem_man.h"
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

    MALLOC(vertex_pages, sizeof(vertex_t *));
    MALLOC(vertex_pages[0], sizeof(vertex_t) * vertices_on_page);
}

vertex_t * new_vertex(void)
{
    return new_vertex_id(get_unique_id());
}

vertex_t * new_vertex_id(vertex_id_t id)
{
    vertex_t *vertex;

    if (next_vertex >= vertices_on_page) {
        REALLOC(vertex_pages, sizeof(vertex_t *) * (pages_size + 1));
        MALLOC(vertex_pages[pages_size], sizeof(vertex_t) * vertices_on_page);

        pages_size += 1;
        next_vertex = 0;
    }

    vertex = &(vertex_pages[pages_size-1][next_vertex]);
    next_vertex += 1;

    vertex->unique_id = id;
    vertex->outgoing_len = 0;
    vertex->outgoing_size = INIT_EDGES_NUM;
    MALLOC(vertex->outgoing, sizeof(edge_t) * INIT_EDGES_NUM);
    vertex->incoming_len = 0;
    vertex->incoming_size = INIT_EDGES_NUM;
    MALLOC(vertex->incoming, sizeof(edge_t) * INIT_EDGES_NUM);
    vertex->label = NULL;
    vertex->visited = 0;
    vertex->graph_index = 0;

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
    if (vertex->outgoing_size != 0)
        free(vertex->outgoing);

    if (vertex->incoming_size != 0)
        free(vertex->incoming);
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

void vertex_remove_outgoing(vertex_t *start, vertex_t const *end)
{
    uint32_t i;
    vertex_t *current_end;

    if (start->outgoing_len == 0)
        return;

    for (i = 0; i < start->outgoing_len; i++) {
        current_end = start->outgoing[i].end;

        if (end == current_end)
            break;
    }

    for (; i < start->outgoing_len - 1; i++)
        start->outgoing[i] = start->outgoing[i + 1];

    start->outgoing_len -= 1;
}

void vertex_remove_incoming(vertex_t *end, vertex_t const *start)
{
    uint32_t i;
    vertex_t *current_end;

    if (end->incoming_len == 0)
        return;

    for (i = 0; i < end->incoming_len; i++) {
        current_end = end->incoming[i].end;

        if (start == current_end)
            break;
    }

    for (; i < end->incoming_len - 1; i++)
        end->incoming[i] = end->incoming[i + 1];

    end->incoming_len -= 1;
}

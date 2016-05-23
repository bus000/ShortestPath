#include "vertex.h"
#include "mem_man.h"
#include "error.h"
#include "vertex_list.h"
#include <stdlib.h>
#include <unistd.h>

/* Returns a different unique ID each time the function is called. */
static vertex_id_t get_unique_id()
{
    static vertex_id_t unique_id = 0;

    unique_id += 1;

    return unique_id;
}

void vertices_init(void)
{

}

vertex_t * new_vertex(void)
{
    return new_vertex_id(get_unique_id());
}

vertex_t * new_vertex_id(vertex_id_t id)
{
    vertex_t *vertex;

    MALLOC(vertex, sizeof(*vertex));

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

void vertex_free(vertex_t *vertex)
{
    if (vertex->outgoing_size != 0)
        FREE(vertex->outgoing);

    if (vertex->incoming_size != 0)
        FREE(vertex->incoming);

    FREE(vertex);
}

void vertices_free(void)
{

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

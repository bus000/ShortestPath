#include "thorup_dist.h"

/* Assumes the graph is connected. */
int layer(graph_t const *graph)
{
    vertex_t *start = find_vertex(graph, 0);
    vertex_list_t layer0;

    /* Label 0 is all vertices reachable from a random vertex. */
    vertex_list_init(&layer0);
    reachable(&layer0, start);

    /* Free resources used. */
    vertex_list_free(&layer0);

    return 0;
}

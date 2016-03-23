#include "error.h"
#include "thorup_dist.h"
#include <stdlib.h>

static int get_two_layer_graphs(graph_t *newgraph, graph_t *oldgraph,
        vertex_t *vertex);

/* Copies all vertices in the vertex list to a new buffer.
 * TODO: handle the vertices edges, which points out of the list. */
static vertex_t * pointer_list_to_list(vertex_list_t const *list);

int thorup_layer(graph_t *graph)
{
    /*vertex_t *start = find_vertex(graph, 0);*/
    /*graph_t *newgraph = malloc(sizeof(graph_t));*/

    /*if (newgraph == NULL)*/
        /*mem_err();*/

    /*graph_init(newgraph);*/

    get_two_layer_graphs(graph, graph, NULL);

    return 0;

}

static int get_two_layer_graphs(graph_t *newgraph, graph_t *oldgraph,
        vertex_t *vertex)
{
    vertex_list_t reach;
    vertex_t *vertices;

    vertex_list_init(&reach);

    /* Make graph of all vertices reachable from start. */
    reachable(&reach, vertex);
    vertices = pointer_list_to_list(&reach);
    graph_init_vertices(newgraph, vertices, reach.len);

    /* Remove all vertices from old graph that are in the newly created
     * graph replacing them with a single new vertex. */

    /* Find all vertices in the old graph that has a transition into the new
     * graph, add these vertices to the new graph and remove them from the
     * old. */

    /* Let the new start vertex be all the vertices in the new graph contracted
     * to a single vertex, repeat all steps. */

    /* Free resources. */
    vertex_list_free(&reach);

    return 0;
}

/* TODO: Change edge pointers to go to correct places. */
static vertex_t * pointer_list_to_list(vertex_list_t const *list)
{
    uint32_t i;
    vertex_t *arr = malloc(sizeof(vertex_t) * list->len);

    if (arr == NULL)
        mem_err();

    for (i = 0; i < list->len; i++)
        arr[i] = *list->vertices[i];

    return arr;
}

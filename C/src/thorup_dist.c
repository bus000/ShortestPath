#include "error.h"
#include "thorup_dist.h"
#include <stdlib.h>

static int get_two_layer_graphs(graph_t *newgraph, graph_t *oldgraph,
        vertex_t *vertex);

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
    vertex_id_t new;
    vertex_list_t reachable_list;
    vertex_list_t reaching_list;

    vertex_list_init(&reachable_list);
    vertex_list_init(&reaching_list);

    /* Make graph of all vertices reachable from start. */
    reachable(&reachable_list, vertex);
    graph_init_vertices(newgraph, reachable_list.vertices, reachable_list.len);
    graph_contract(oldgraph, &reachable_list);

    /* Remove all vertices from old graph that are in the newly created
     * graph replacing them with a single new vertex, "new". */
    new = graph_contract(oldgraph, &reachable_list);

    printf("%u\n", new);

    /* Find all vertices in the old graph that has a transition into the new
     * graph, add these vertices to the new graph and remove them from the
     * old. */

    /* Let the new start vertex be all the vertices in the new graph contracted
     * to a single vertex, repeat all steps. */

    /* Free resources. */
    vertex_list_free(&reachable_list);
    vertex_list_free(&reaching_list);

    return 0;
}

#include "graph.h"
#include "map.h"
#include "queue.h"
#include "graph_labeling.h"

static int hashvertex(void const *v)
{
    vertex_t const *vertex = v;

    return vertex->unique_id;
}

static int cmpvertices(void const *v1, void const *v2)
{
    vertex_t const *vertex1 = v1;
    vertex_t const *vertex2 = v2;

    if (vertex1->unique_id < vertex2->unique_id)
        return -1;
    else if (vertex1->unique_id > vertex2->unique_id)
        return 1;
    else
        return 0;
}

spanning_tree_t graph_bf_spanning_tree(digraph_t *graph)
{
    uint32_t i;
    vertex_t *first = graph_first_vertex(graph), *current, *adjasent;
    queue_t queue;
    spanning_tree_t tree = { .vertex = first,
        .neighbours = linked_list_init() };
    map_t parents;

    /*graph_init_labels(graph, &default_label, sizeof(default_label));*/

    if (first == NULL)
        return tree;

    queue = queue_singular(first);
    map_init(&parents, 128, hashvertex, cmpvertices);

    while ((current = (vertex_t *) dequeue(&queue)) != NULL) {

        for (i = 0; i < current->outgoing_len; i++) {
            adjasent = current->outgoing[i].end;

            map_put(&parents, adjasent, current);
        }
    }

    return tree;
}

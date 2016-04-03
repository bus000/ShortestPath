#include "graph.h"
#include "vertex.h"
#include "map.h"
#include "graph_labeling.h"
#include <string.h>

static int hash(void const *v);
static int cmp_keys(void const *v1, void const *v2);

graph_t graph_copy(graph_t const *graph)
{
    uint32_t i, j;
    graph_t new_graph;
    map_t map;
    vertex_t *vertex, *vertex_new, *edge_end;
    edge_t edge;

    map_init(&map, 64, hash, cmp_keys);
    graph_init(&new_graph);

    if (graph->label_size != 0)
        graph_init_labels_size(&new_graph,
                graph->labels_size * graph->label_size, graph->label_size);

    /* Create same number of vertices in new graph as in old graph. Connecting
     * the old graph vertex to the new one in the map. */
    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        vertex_new = new_vertex();
        map_put(&map, vertex, vertex_new);
        graph_add_vertex_pointer(&new_graph, vertex_new);

        if (graph->label_size != 0) {
            vertex_new->label = new_graph.labels + i * new_graph.label_size;
            memcpy(vertex_new->label, vertex->label, graph->label_size);
        }
    }

    /* Add edges. */
    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        vertex_new = (vertex_t *) map_get(&map, vertex);

        for (j = 0; j < vertex->edges_len; j++) {
            edge = vertex->edges[j];
            edge_end = (vertex_t *) map_get(&map, edge.end);

            graph_add_edge(&new_graph, vertex_new->unique_id,
                    edge_end->unique_id, edge.weight);
        }
    }

    /* Free resources from the heap. */
    map_free(&map);

    return new_graph;
}

static int hash(void const *v)
{
    vertex_t const *vertex = (vertex_t const *) v;

    return (int) vertex->unique_id;
}

static int cmp_keys(void const *v1, void const *v2)
{
    vertex_t const *vertex1 = (vertex_t const *) v1;
    vertex_t const *vertex2 = (vertex_t const *) v2;

    return !(vertex1->unique_id == vertex2->unique_id);
}

#include "../src/graph.h"
#include "../src/vertex.h"
#include <stdlib.h>

static int vertex_compare(void const *el1, void const *el2);

int main(int argc, char const *argv[])
{
    int i;
    digraph_t graph;
    vertex_id_t vertices[10] = { 0 };
    vertex_list_t vertex_list;
    vertex_t *vertex;

    /* Initialize graph. */
    vertices_init();
    graph_init(&graph);
    vertex_list_init(&vertex_list);

    for (i = 0; i < 10; i++)
        vertices[i] = graph_add_vertex(&graph);

    graph_add_edge(&graph, vertices[0], vertices[8], 10);
    graph_add_edge(&graph, vertices[8], vertices[1], 10);
    graph_add_edge(&graph, vertices[8], vertices[3], 10);
    graph_add_edge(&graph, vertices[6], vertices[5], 10);
    graph_add_edge(&graph, vertices[5], vertices[8], 10);
    graph_add_edge(&graph, vertices[3], vertices[2], 10);
    graph_add_edge(&graph, vertices[4], vertices[3], 10);
    graph_add_edge(&graph, vertices[3], vertices[7], 10);
    graph_add_edge(&graph, vertices[4], vertices[7], 10);
    graph_add_edge(&graph, vertices[9], vertices[7], 10);

    vertex = find_vertex(&graph, vertices[0]);

    /* Find vertices reachable from vertex 0, should be [0, 8, 1, 3, 2, 7]. */
    reachable(&vertex_list, vertex);
    qsort(vertex_list.vertices, vertex_list.len, sizeof(vertex_t *),
            vertex_compare);

    printf("[");
    for (i = 0; i < vertex_list.len - 1; i++) {
        printf("%u, ", vertex_list.vertices[i]->unique_id);
    }
    printf("%u]\n", vertex_list.vertices[i]->unique_id);

    /* Free resources. */
    graph_free(&graph);
    vertex_list_free(&vertex_list);

    return 0;
}

static int vertex_compare(void const *el1, void const *el2)
{
    vertex_t const *vertex1 = (vertex_t const *) el1;
    vertex_t const *vertex2 = (vertex_t const *) el2;

    return vertex1->unique_id - vertex2->unique_id;
}

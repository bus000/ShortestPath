#include "../src/graph.h"
#include "../src/vertex.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    digraph_t graph;
    vertex_id_t v0, v1, v2, v3, v4, v5;
    path_t path;
    vertex_t *vertex0, *vertex5;

    vertices_init();
    graph_init(&graph);

    /* Create test graph. */
    v0 = graph_add_vertex(&graph);
    v1 = graph_add_vertex(&graph);
    v2 = graph_add_vertex(&graph);
    v3 = graph_add_vertex(&graph);
    v4 = graph_add_vertex(&graph);
    v5 = graph_add_vertex(&graph);

    graph_add_edge(&graph, v0, v1, 5);
    graph_add_edge(&graph, v0, v3, 1);
    graph_add_edge(&graph, v1, v2, 2);
    graph_add_edge(&graph, v1, v3, 7);
    graph_add_edge(&graph, v2, v1, 4);
    graph_add_edge(&graph, v2, v5, 4);
    graph_add_edge(&graph, v3, v2, 9);
    graph_add_edge(&graph, v3, v4, 12);
    graph_add_edge(&graph, v4, v2, 3);
    graph_add_edge(&graph, v4, v5, 2);

    vertex0 = find_vertex(&graph, v0);
    vertex5 = find_vertex(&graph, v5);
    dijkstra(&path, &graph, vertex0, vertex5);

    printf("path length %u\n", path.length);

    path_free(&path);
    graph_free(&graph);
    vertices_free();

    return EXIT_SUCCESS;
}

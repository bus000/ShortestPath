#include "../src/graph.h"
#include "../src/planarity.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    uint32_t i;
    digraph_t graph;
    vertex_id_t vertices[100] = { 0 };

    vertices_init();
    graph_init(&graph);

    for (i = 1; i < 6; i++)
        vertices[i] = graph_add_vertex(&graph);

    graph_add_edge(&graph, vertices[1], vertices[2], 1);
    graph_add_edge(&graph, vertices[1], vertices[3], 1);
    graph_add_edge(&graph, vertices[1], vertices[4], 1);
    graph_add_edge(&graph, vertices[1], vertices[5], 1);
    graph_add_edge(&graph, vertices[2], vertices[3], 1);
    graph_add_edge(&graph, vertices[2], vertices[5], 1);
    graph_add_edge(&graph, vertices[2], vertices[4], 1);
    graph_add_edge(&graph, vertices[3], vertices[4], 1);
    graph_add_edge(&graph, vertices[3], vertices[5], 1);
    graph_add_edge(&graph, vertices[4], vertices[5], 1);

    switch (planar(&graph)) {
    case -3:
        printf("Graph not planar\n");
        break;
    case -2:
        printf("Biconnected failed\n");
        break;
    case -1:
        printf("Could not find vertex with palm number 0\n");
        break;
    case 0:
        printf("Graph is planar\n");
        break;
    }

    graph_free(&graph);
    vertices_free();

    return EXIT_SUCCESS;
}

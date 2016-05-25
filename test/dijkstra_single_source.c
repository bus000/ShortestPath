#include "../src/path.h"
#include "../src/dijkstra.h"
#include "../src/graph.h"
#include <stdio.h>
#include <stdlib.h>

static void reaches(dijkstra_oracle_t const *oracle, vertex_id_t v)
{
    vertex_t *vertex = find_vertex(oracle->graph, v);

    if (dijkstra_reaches(oracle, vertex)) {
        printf("%u reaches %u\n", oracle->source->unique_id, v);
    } else {
        printf("%u does not reach %u\n", oracle->source->unique_id, v);
    }
}

int main(int argc, char const *argv[])
{
    uint32_t i;
    digraph_t graph;
    vertex_t *vertex0;
    dijkstra_oracle_t oracle;
    vertex_id_t vertices[14] = { 0 };

    vertices_init();
    graph_init(&graph);

    /* Create test graph. */
    for (i = 1; i < 13; i++)
        vertices[i] = graph_add_vertex(&graph);

    graph_add_edge(&graph, vertices[1], vertices[2], 2);
    graph_add_edge(&graph, vertices[1], vertices[3], 2);
    graph_add_edge(&graph, vertices[1], vertices[10], 2);
    graph_add_edge(&graph, vertices[10], vertices[4], 2);
    graph_add_edge(&graph, vertices[11], vertices[10], 2);
    graph_add_edge(&graph, vertices[4], vertices[5], 2);
    graph_add_edge(&graph, vertices[3], vertices[4], 2);
    graph_add_edge(&graph, vertices[6], vertices[3], 2);
    graph_add_edge(&graph, vertices[6], vertices[2], 2);
    graph_add_edge(&graph, vertices[2], vertices[7], 2);
    graph_add_edge(&graph, vertices[9], vertices[7], 2);
    graph_add_edge(&graph, vertices[7], vertices[8], 2);
    graph_add_edge(&graph, vertices[8], vertices[9], 2);

    vertex0 = find_vertex(&graph, vertices[1]);
    oracle = dijkstra_init(&graph, vertex0);

    reaches(&oracle, vertices[3]);
    reaches(&oracle, vertices[9]);
    reaches(&oracle, vertices[11]);
    reaches(&oracle, vertices[6]);
    reaches(&oracle, vertices[5]);
    reaches(&oracle, vertices[1]);
    reaches(&oracle, vertices[2]);
    reaches(&oracle, vertices[8]);

    dijkstra_free(&oracle);

    return EXIT_SUCCESS;
}

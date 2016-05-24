#include "../src/graph.h"
#include "../src/tabular_reachability.h"
#include <stdio.h>
#include <stdlib.h>

static digraph_t construct_graph()
{
    uint32_t i;
    vertex_id_t vertex_ids[14] = { 0 };
    digraph_t graph;

    vertices_init();
    graph_init(&graph);

    for (i = 1; i < 14; i++)
        vertex_ids[i] = graph_add_vertex(&graph);

    graph_add_edge(&graph, vertex_ids[1], vertex_ids[2], 2);

    graph_add_edge(&graph, vertex_ids[2], vertex_ids[6], 2);
    graph_add_edge(&graph, vertex_ids[2], vertex_ids[7], 2);

    graph_add_edge(&graph, vertex_ids[3], vertex_ids[1], 2);
    graph_add_edge(&graph, vertex_ids[3], vertex_ids[2], 2);
    graph_add_edge(&graph, vertex_ids[3], vertex_ids[12], 2);

    graph_add_edge(&graph, vertex_ids[4], vertex_ids[5], 2);
    graph_add_edge(&graph, vertex_ids[4], vertex_ids[10], 2);

    graph_add_edge(&graph, vertex_ids[5], vertex_ids[4], 2);

    graph_add_edge(&graph, vertex_ids[6], vertex_ids[5], 2);
    graph_add_edge(&graph, vertex_ids[6], vertex_ids[7], 2);

    graph_add_edge(&graph, vertex_ids[8], vertex_ids[11], 2);
    graph_add_edge(&graph, vertex_ids[8], vertex_ids[13], 2);
    graph_add_edge(&graph, vertex_ids[8], vertex_ids[9], 2);

    graph_add_edge(&graph, vertex_ids[9], vertex_ids[4], 2);
    graph_add_edge(&graph, vertex_ids[9], vertex_ids[10], 2);
    graph_add_edge(&graph, vertex_ids[9], vertex_ids[8], 2);

    graph_add_edge(&graph, vertex_ids[11], vertex_ids[3], 2);
    graph_add_edge(&graph, vertex_ids[11], vertex_ids[8], 2);
    graph_add_edge(&graph, vertex_ids[11], vertex_ids[13], 2);

    graph_add_edge(&graph, vertex_ids[12], vertex_ids[7], 2);

    graph_add_edge(&graph, vertex_ids[13], vertex_ids[12], 2);

    return graph;
}

int main(int argc, char const *argv[])
{
    uint32_t i, j;
    vertex_t *v1, *v2;
    digraph_t graph = construct_graph();
    table_reachability_t table = table_init(&graph);

    for (i = 0; i < graph.vertices_len; i++) {
        v1 = graph.vertices[i];

        for (j = 0; j < graph.vertices_len-1; j++) {
            v2 = graph.vertices[j];

            printf("%d ", table_reaches(&table, v1, v2));
        }
        v2 = graph.vertices[j];
        printf("%d\n", table_reaches(&table, v1, v2));
    }

    table_free(&table);

    graph_free(&graph);
    vertices_free();

    return EXIT_SUCCESS;
}

#include "../src/thorup_dist.h"
#include "../src/graph.h"
#include <stdio.h>

int main(int argc, char const *argv[])
{
    uint32_t i;
    graph_t graph;
    vertex_id_t vertices[10] = { 0 };
    reachability_oracle_t oracle;
    vertex_t *vertex;

    vertices_init();
    graph_init(&graph);

    for (i = 0; i < 10; i++)
        vertices[i] = graph_add_vertex(&graph);

    graph_add_edge(&graph, vertices[0], vertices[3], 1);
    graph_add_edge(&graph, vertices[1], vertices[0], 1);
    graph_add_edge(&graph, vertices[2], vertices[0], 1);
    graph_add_edge(&graph, vertices[2], vertices[6], 1);
    graph_add_edge(&graph, vertices[3], vertices[0], 1);
    graph_add_edge(&graph, vertices[4], vertices[0], 1);
    graph_add_edge(&graph, vertices[4], vertices[5], 1);
    graph_add_edge(&graph, vertices[4], vertices[9], 1);
    graph_add_edge(&graph, vertices[7], vertices[2], 1);
    graph_add_edge(&graph, vertices[9], vertices[8], 1);

    thorup_reach_oracle(&oracle, &graph);
    graph_free(&graph);

    printf("graphs number = %u\n", oracle.graphs_len);
    printf("graphs 1 number = %u\n", oracle.graphs[0].vertices_len);
    printf("graphs 2 number = %u\n", oracle.graphs[1].vertices_len);

    graph = oracle.graphs[0];
    for (i = 0; i < graph.vertices_len; i++) {
        vertex = graph.vertices[i];
        printf("containing vertex %u\n", vertex->unique_id);
    }

    reach_oracle_free(&oracle);
    vertices_free();

    return 0;
}

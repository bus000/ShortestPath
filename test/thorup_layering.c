#include "../src/thorup_dist.h"
#include "../src/graph.h"
#include <stdio.h>
#include <stdlib.h>

#define VERTEX_NUMBER (32)

static digraph_t construct_graph()
{
    uint32_t i;
    digraph_t graph;
    vertex_id_t vertices[VERTEX_NUMBER + 1] = { 0 };

    vertices_init();
    graph_init(&graph);

    for (i = 1; i < VERTEX_NUMBER + 1; i++)
        vertices[i] = graph_add_vertex(&graph);

    graph_add_edge(&graph, vertices[1], vertices[2], 2);
    graph_add_edge(&graph, vertices[1], vertices[3], 2);
    graph_add_edge(&graph, vertices[1], vertices[4], 2);
    graph_add_edge(&graph, vertices[1], vertices[5], 2);
    graph_add_edge(&graph, vertices[6], vertices[2], 2);
    graph_add_edge(&graph, vertices[6], vertices[14], 2);
    graph_add_edge(&graph, vertices[6], vertices[15], 2);
    graph_add_edge(&graph, vertices[6], vertices[29], 2);
    graph_add_edge(&graph, vertices[7], vertices[2], 2);
    graph_add_edge(&graph, vertices[7], vertices[3], 2);
    graph_add_edge(&graph, vertices[7], vertices[15], 2);
    graph_add_edge(&graph, vertices[7], vertices[16], 2);
    graph_add_edge(&graph, vertices[7], vertices[17], 2);
    graph_add_edge(&graph, vertices[8], vertices[3], 2);
    graph_add_edge(&graph, vertices[8], vertices[17], 2);
    graph_add_edge(&graph, vertices[8], vertices[18], 2);
    graph_add_edge(&graph, vertices[8], vertices[19], 2);
    graph_add_edge(&graph, vertices[9], vertices[3], 2);
    graph_add_edge(&graph, vertices[9], vertices[4], 2);
    graph_add_edge(&graph, vertices[9], vertices[19], 2);
    graph_add_edge(&graph, vertices[9], vertices[20], 2);
    graph_add_edge(&graph, vertices[9], vertices[21], 2);
    graph_add_edge(&graph, vertices[10], vertices[4], 2);
    graph_add_edge(&graph, vertices[10], vertices[21], 2);
    graph_add_edge(&graph, vertices[10], vertices[22], 2);
    graph_add_edge(&graph, vertices[10], vertices[23], 2);
    graph_add_edge(&graph, vertices[11], vertices[4], 2);
    graph_add_edge(&graph, vertices[11], vertices[5], 2);
    graph_add_edge(&graph, vertices[11], vertices[23], 2);
    graph_add_edge(&graph, vertices[11], vertices[24], 2);
    graph_add_edge(&graph, vertices[11], vertices[25], 2);
    graph_add_edge(&graph, vertices[12], vertices[5], 2);
    graph_add_edge(&graph, vertices[12], vertices[25], 2);
    graph_add_edge(&graph, vertices[12], vertices[26], 2);
    graph_add_edge(&graph, vertices[12], vertices[27], 2);
    graph_add_edge(&graph, vertices[13], vertices[2], 2);
    graph_add_edge(&graph, vertices[13], vertices[5], 2);
    graph_add_edge(&graph, vertices[13], vertices[27], 2);
    graph_add_edge(&graph, vertices[13], vertices[28], 2);
    graph_add_edge(&graph, vertices[13], vertices[29], 2);
    graph_add_edge(&graph, vertices[30], vertices[16], 2);
    graph_add_edge(&graph, vertices[30], vertices[31], 2);
    graph_add_edge(&graph, vertices[32], vertices[31], 2);

    return graph;
}

int main(int argc, char const *argv[])
{
    uint32_t i;
    digraph_t graph = construct_graph();
    vertex_t *vertex;
    thorup_label_t *thorup_label;

    layering(&graph);

    for (i = 0; i < graph.vertices_len; i++) {
        vertex = graph.vertices[i];
        thorup_label = vertex->label;

        printf("vertex %u has layer %u\n", vertex->unique_id, thorup_label->layer);
    }

    return EXIT_SUCCESS;
}

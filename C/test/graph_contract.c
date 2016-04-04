#include "../src/graph.h"
#include "../src/vertex.h"
#include <stdlib.h>
#include <stdio.h>

static int vertex_compare(void const *el1, void const *el2);

int main(int argc, char const *argv[])
{
    uint32_t i;
    graph_t graph;
    vertex_id_t vertices[14] = { 0 };
    vertex_list_t vertex_list;

    vertices_init();
    graph_init(&graph);
    vertex_list_init(&vertex_list);

    /* Create test graph. */
    vertices[1] = graph_add_vertex(&graph);
    vertices[2] = graph_add_vertex(&graph);
    vertices[3] = graph_add_vertex(&graph);
    vertices[4] = graph_add_vertex(&graph);
    vertices[5] = graph_add_vertex(&graph);
    vertices[6] = graph_add_vertex(&graph);
    vertices[7] = graph_add_vertex(&graph);
    vertices[8] = graph_add_vertex(&graph);
    vertices[9] = graph_add_vertex(&graph);
    vertices[10] = graph_add_vertex(&graph);
    vertices[11] = graph_add_vertex(&graph);
    vertices[12] = graph_add_vertex(&graph);
    vertices[13] = graph_add_vertex(&graph);

    graph_add_edge(&graph, vertices[1], vertices[2], 1);
    graph_add_edge(&graph, vertices[1], vertices[4], 1);
    graph_add_edge(&graph, vertices[1], vertices[5], 1);
    graph_add_edge(&graph, vertices[1], vertices[3], 1);
    graph_add_edge(&graph, vertices[2], vertices[4], 1);
    graph_add_edge(&graph, vertices[3], vertices[1], 1);
    graph_add_edge(&graph, vertices[3], vertices[2], 1);
    graph_add_edge(&graph, vertices[4], vertices[1], 1);
    graph_add_edge(&graph, vertices[4], vertices[5], 1);
    graph_add_edge(&graph, vertices[5], vertices[10], 1);
    graph_add_edge(&graph, vertices[5], vertices[3], 1);
    graph_add_edge(&graph, vertices[6], vertices[2], 1);
    graph_add_edge(&graph, vertices[6], vertices[13], 1);
    graph_add_edge(&graph, vertices[6], vertices[7], 1);
    graph_add_edge(&graph, vertices[7], vertices[8], 1);
    graph_add_edge(&graph, vertices[8], vertices[3], 1);
    graph_add_edge(&graph, vertices[8], vertices[9], 1);
    graph_add_edge(&graph, vertices[9], vertices[10], 1);
    graph_add_edge(&graph, vertices[11], vertices[10], 1);
    graph_add_edge(&graph, vertices[12], vertices[4], 1);
    graph_add_edge(&graph, vertices[12], vertices[11], 1);
    graph_add_edge(&graph, vertices[13], vertices[12], 1);

    /* Get subset of vertices. */
    reachable(&vertex_list, find_vertex(&graph, vertices[1]));
    qsort(vertex_list.vertices, vertex_list.len, sizeof(vertex_t *),
            vertex_compare);

    printf("[");
    for (i = 0; i < vertex_list.len - 1; i++) {
        printf("%u, ", vertex_list.vertices[i]->unique_id);
    }
    printf("%u]\n", vertex_list.vertices[i]->unique_id);

    /* Contract all vertices reachable from vertex1 to a single vertex. */
    graph_contract(&graph, &vertex_list);

    printf("graph.vertices_len = %u\n", graph.vertices_len);

    /* Verify that 6, 8, 9, 11, and 12 points to the new vertex. */
    vertex_t *vertex9 = find_vertex(&graph, vertices[9]);
    printf("vertex 9 edge list length = %u\n", vertex9->edges_len);
    vertex_t *new = vertex9->edges[0].end;

    if (find_vertex(&graph, vertices[11])->edges[0].end == new)
        printf("11 points to new\n");

    if (find_vertex(&graph, vertices[12])->edges[0].end == new ||
            find_vertex(&graph, vertices[12])->edges[1].end == new)
        printf("12 points to new\n");

    if (find_vertex(&graph, vertices[6])->edges[0].end == new ||
            find_vertex(&graph, vertices[6])->edges[1].end == new)
        printf("6 points to new\n");

    /* Verify that the new vertex points to nothing. */
    printf("new length %u\n", new->edges_len);

    graph_free(&graph);
    vertex_list_free(&vertex_list);
    vertices_free();

    return 0;
}

static int vertex_compare(void const *el1, void const *el2)
{
    vertex_t const *vertex1 = (vertex_t const *) el1;
    vertex_t const *vertex2 = (vertex_t const *) el2;

    return vertex1->unique_id - vertex2->unique_id;
}

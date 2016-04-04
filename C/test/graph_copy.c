#include "graph.h"
#include <stdio.h>

int main(int argc, char const *argv[])
{
    uint32_t i;
    graph_t graph, cpy;
    vertex_id_t vertices[6] = { 0 };

    vertices_init();

    graph_init(&graph);

    for (i = 1; i < 6; i++)
        vertices[i] = graph_add_vertex(&graph);

    graph_add_edge(&graph, vertices[1], vertices[2], 2);
    graph_add_edge(&graph, vertices[1], vertices[5], 2);
    graph_add_edge(&graph, vertices[2], vertices[3], 2);
    graph_add_edge(&graph, vertices[3], vertices[4], 2);
    graph_add_edge(&graph, vertices[4], vertices[1], 2);

    cpy = graph_copy(&graph);

    graph_free(&graph);

    vertex_t *vertex1 = find_vertex(&cpy, 1);
    vertex_t *vertex2 = find_vertex(&cpy, 2);
    vertex_t *vertex3 = find_vertex(&cpy, 3);
    vertex_t *vertex4 = find_vertex(&cpy, 4);
    vertex_t *vertex5 = find_vertex(&cpy, 5);

    printf("vertex1 edges %u\n", vertex1->edges_len);
    printf("vertex2 edges %u\n", vertex2->edges_len);
    printf("vertex3 edges %u\n", vertex3->edges_len);
    printf("vertex4 edges %u\n", vertex4->edges_len);
    printf("vertex5 edges %u\n", vertex5->edges_len);

    printf("\nvertex1:\n");
    for (i = 0; i < vertex1->edges_len; i++)
        printf("%u -> %u\n", vertex1->edges[i].start->unique_id,
                vertex1->edges[i].end->unique_id);

    printf("\nvertex2:\n");
    for (i = 0; i < vertex2->edges_len; i++)
        printf("%u -> %u\n", vertex2->edges[i].start->unique_id,
                vertex2->edges[i].end->unique_id);

    printf("\nvertex3:\n");
    for (i = 0; i < vertex3->edges_len; i++)
        printf("%u -> %u\n", vertex3->edges[i].start->unique_id,
                vertex3->edges[i].end->unique_id);

    printf("\nvertex4:\n");
    for (i = 0; i < vertex4->edges_len; i++)
        printf("%u -> %u\n", vertex4->edges[i].start->unique_id,
                vertex4->edges[i].end->unique_id);

    printf("\nvertex5:\n");
    for (i = 0; i < vertex5->edges_len; i++)
        printf("%u -> %u\n", vertex5->edges[i].start->unique_id,
                vertex5->edges[i].end->unique_id);

    graph_free(&cpy);
    vertices_free();

    return 0;
}

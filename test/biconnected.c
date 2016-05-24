#include "../src/graph.h"
#include "../src/bicomponents.h"
#include <stdlib.h>
#include <stdio.h>

static int edge_end_cmp(void const *el1, void const *el2)
{
    edge_t const *edge1 = (edge_t const *) el1;
    edge_t const *edge2 = (edge_t const *) el2;
    vertex_t const *vertex1 = edge1->end;
    vertex_t const *vertex2 = edge2->end;

    if (vertex1->unique_id < vertex2->unique_id)
        return -1;
    else if (vertex1->unique_id > vertex2->unique_id)
        return 1;
    else
        return 0;
}

static void print_vertex(digraph_t const *graph, vertex_id_t id)
{
    uint32_t i;
    vertex_t const *vertex;

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];

        if (vertex->unique_id == id) {
            break;
        }

        vertex = NULL;
    }

    if (vertex == NULL) {
        printf("Could not find vertex with id %u\n", id);

        return;
    }

    /* Sort edges by their end vertex. */
    qsort(vertex->outgoing, vertex->outgoing_len, sizeof(edge_t), edge_end_cmp);

    for (i = 0; i < vertex->outgoing_len; i++) {
        printf("%u -> %u\n", vertex->unique_id,
                vertex->outgoing[i].end->unique_id);
    }
}

int main(int argc, char const *argv[])
{
    uint32_t i;
    digraph_t graph, *first;
    vertex_id_t vertices[100] = { 0 };
    linked_list_t biconnected;

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

    biconnected = biconnect(&graph);

    printf("Number of connected bicomponents %" PRIu64 "\n", biconnected.len);

    /* Find the different vertices outputting their edges. */
    first = (digraph_t *) biconnected.start->element;
    print_vertex(first, 1);
    print_vertex(first, 2);
    print_vertex(first, 3);
    print_vertex(first, 4);
    print_vertex(first, 5);

    /* Free resources. */
    graph_free(first);
    linked_list_free(&biconnected);
    vertices_free();

    return EXIT_SUCCESS;
}

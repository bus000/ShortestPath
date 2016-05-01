#include "../src/palm_tree.h"
#include "../src/graph.h"
#include <stdio.h>
#include <stdlib.h>

void print_palm(digraph_t *graph);
static linked_list_t spine(vertex_t **w, vertex_t const *u);

int main(int argc, char const *argv[])
{
    uint32_t i;
    digraph_t graph;
    vertex_id_t vertices[7] = { 0 };
    linked_list_t spi;
    actual_list_t *it;
    vertex_t *v, *w;

    vertices_init();
    graph_init(&graph);

    for (i = 0; i < 7; i++)
        vertices[i] = graph_add_vertex(&graph);

    graph_add_edge(&graph, vertices[0], vertices[1], 2);
    graph_add_edge(&graph, vertices[0], vertices[2], 2);
    graph_add_edge(&graph, vertices[0], vertices[4], 2);
    graph_add_edge(&graph, vertices[1], vertices[2], 2);
    graph_add_edge(&graph, vertices[1], vertices[3], 2);
    graph_add_edge(&graph, vertices[2], vertices[3], 2);
    graph_add_edge(&graph, vertices[2], vertices[4], 2);
    graph_add_edge(&graph, vertices[2], vertices[6], 2);
    graph_add_edge(&graph, vertices[4], vertices[5], 2);
    graph_add_edge(&graph, vertices[5], vertices[6], 2);

    palm_tree(&graph);

    print_palm(&graph);

    /* Print spines. */
    for (i = 0; i < graph.vertices_len; i++) {
        v = graph.vertices[i];
        w = v->outgoing[0].end;
        spi = spine(&w, v);

        printf("spine: %u -> ", v->unique_id);
        for (it = spi.start; it != NULL; it = it->next) {
            printf("%u -> ", ((vertex_t *) it->element)->unique_id);
        }
        printf("%u\n", w->unique_id);
    }

    graph_free(&graph);
    vertices_free();

    return EXIT_SUCCESS;
}

void print_palm(digraph_t *graph)
{
    uint32_t i, j;
    vertex_t *vertex, *adjasent;
    edge_t edge;

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];

        for (j = 0; j < vertex->outgoing_len; j++) {
            edge = vertex->outgoing[j];
            adjasent = edge.end;

            if (palm_tree_arc(&edge)) {
                printf("%u -> %u is arc\n", vertex->unique_id,
                        adjasent->unique_id);
            } else if (palm_tree_frond(&edge)) {
                printf("%u -> %u is frond\n", vertex->unique_id,
                        adjasent->unique_id);
            } else {
                printf("%u -> %u is neither\n", vertex->unique_id,
                        adjasent->unique_id);
            }
        }
    }
}

static linked_list_t spine(vertex_t **w, vertex_t const *u)
{
    linked_list_t spi = linked_list_init();

    while (palm_number(*w) > palm_number(u)) {
            linked_list_add_end(&spi, *w);
            *w = (*w)->outgoing[0].end;
        }

    return spi;
}

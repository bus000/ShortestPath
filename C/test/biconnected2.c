#include "../src/bicomponents.h"
#include "../src/graph.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static void remove_random_vertex(digraph_t *graph);

int main(int argc, char const *argv[])
{
    uint32_t i;
    digraph_t graph, *element;
    vertex_id_t vertices[11] = { 0 };
    linked_list_t graphs;
    actual_list_t *cur;

    vertices_init();
    graph_init(&graph);

    for (i = 1; i < 11; i++)
        vertices[i] = graph_add_vertex(&graph);

    graph_add_edge(&graph, vertices[1], vertices[2], 2);
    graph_add_edge(&graph, vertices[1], vertices[7], 2);
    graph_add_edge(&graph, vertices[2], vertices[3], 2);
    graph_add_edge(&graph, vertices[2], vertices[4], 2);
    graph_add_edge(&graph, vertices[3], vertices[5], 2);
    graph_add_edge(&graph, vertices[5], vertices[4], 2);
    graph_add_edge(&graph, vertices[4], vertices[3], 2);
    graph_add_edge(&graph, vertices[2], vertices[6], 2);
    graph_add_edge(&graph, vertices[6], vertices[7], 2);
    graph_add_edge(&graph, vertices[6], vertices[8], 2);
    graph_add_edge(&graph, vertices[6], vertices[9], 2);
    graph_add_edge(&graph, vertices[8], vertices[9], 2);
    graph_add_edge(&graph, vertices[9], vertices[10], 2);

    graphs = biconnect(&graph);

    for (cur = graphs.start; cur != NULL; cur = cur->next) {
        element = (digraph_t *) cur->element;

        if (connected_undirected(element))
            printf("graph is connected\n");
        else
            printf("graph is unconnected\n");

        for (i = 0; i < element->vertices_len; i++) {
            printf("%u\n", element->vertices[i]->unique_id);
        }

        /* Remove random edge, since graph is biconnected, the graph should
         * still be connected in the undirected case. */
        remove_random_vertex(element);

        if (connected_undirected(element))
            printf("still graph is connected\n");
        else
            printf("still graph is unconnected\n");

        graph_free(element);
        free(element);

        printf("\n");
    }

    linked_list_free(&graphs);
    graph_free(&graph);
    vertices_free();

    return 0;
}

static void remove_random_vertex(digraph_t *graph)
{
    vertex_t *random_vertex;
    uint32_t random_vertex_n;
    vertex_list_t vertex_list;

    srand(time(NULL));

    random_vertex_n = rand() % graph->vertices_len;
    random_vertex = graph->vertices[random_vertex_n];

    vertex_list_init_singular(&vertex_list, random_vertex);

    graph_remove_vertices(graph, &vertex_list);
}

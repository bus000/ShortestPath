#include "../src/graph.h"
#include "../src/vertex.h"
#include <stdio.h>
#include <stdlib.h>

static int vertex_compare(void const *el1, void const *el2);

int main(int argc, char const *argv[])
{
    uint32_t i;
    digraph_t graph;
    vertex_list_t reaching_list;
    vertex_id_t vertices[9] = { 0 };
    vertex_id_t new;

    vertices_init();
    graph_init(&graph);
    vertex_list_init(&reaching_list);

    for (i = 0; i < 9; i++)
        vertices[i] = graph_add_vertex(&graph);

    graph_add_edge(&graph, vertices[1], vertices[0], 1);
    graph_add_edge(&graph, vertices[1], vertices[6], 1);
    graph_add_edge(&graph, vertices[1], vertices[4], 1);
    graph_add_edge(&graph, vertices[2], vertices[3], 1);
    graph_add_edge(&graph, vertices[2], vertices[8], 1);
    graph_add_edge(&graph, vertices[2], vertices[5], 1);
    graph_add_edge(&graph, vertices[3], vertices[0], 1);
    graph_add_edge(&graph, vertices[3], vertices[7], 1);
    graph_add_edge(&graph, vertices[3], vertices[8], 1);
    graph_add_edge(&graph, vertices[4], vertices[0], 1);
    graph_add_edge(&graph, vertices[5], vertices[2], 1);
    graph_add_edge(&graph, vertices[8], vertices[7], 1);

    /* Get subset of vertices. */
    reaching(&reaching_list, find_vertex(&graph, vertices[0]), &graph);
    qsort(reaching_list.vertices, reaching_list.len, sizeof(vertex_t *),
            vertex_compare);

    printf("[");
    for (i = 0; i < reaching_list.len - 1; i++) {
        printf("%u, ", reaching_list.vertices[i]->unique_id);
    }
    printf("%u]\n", reaching_list.vertices[i]->unique_id);

    /* Remove all vertices in list from graph. */
    new = graph_contract(&graph, &reaching_list);
    vertex_t *newvertex = find_vertex(&graph, new);
    edge_t edge;
    vertex_id_t edge_end;

    for (i = 0; i < newvertex->outgoing_len; i++) {
        edge = newvertex->outgoing[i];
        edge_end = edge.end->unique_id;
        if (edge_end == vertices[6] || edge_end == vertices[7] ||
                edge_end == vertices[8])
            printf("found\n");
    }

    vertex_list_free(&reaching_list);
    graph_free(&graph);
    vertices_free();

    return 0;
}

static int vertex_compare(void const *el1, void const *el2)
{
    vertex_t const *vertex1 = (vertex_t const *) el1;
    vertex_t const *vertex2 = (vertex_t const *) el2;

    return vertex1->unique_id - vertex2->unique_id;
}

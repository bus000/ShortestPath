#include "bicomponents.h"
#include "stack.h"
#include "graph_labeling.h"
#include "util.h"
#include "error.h"
#include "map.h"
#include <stdlib.h>

static biconnect_label_t default_label = { .parent = NULL, .d = 0, .low = 0 };

/* Performs a depth first search constructing the biconnected components along
 * the way. */
static void dfs_visit(vertex_t *vertex, uint32_t *count, stack_t *stack,
        linked_list_t *graphs);

/* Outputs a found biconnected component from the stack. */
/* Constructs a graph from a found biconnected component on the stack. */
static digraph_t output_comp(vertex_t *vertex1, vertex_t *vertex2,
        stack_t *stack);

linked_list_t biconnect(digraph_t *graph)
{
    uint32_t count = 0, i;
    vertex_t *vertex;
    stack_t stack = stack_init(graph->vertices_len);
    linked_list_t graphs;

    graph_init_labels(graph, &default_label, sizeof(biconnect_label_t));
    linked_list_init(&graphs);

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];

        if (!vertex->visited)
            dfs_visit(vertex, &count, &stack, &graphs);
    }

    /* Set all visited to 0. */
    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        vertex->visited = 0;
    }

    /* Free resources used by function. */
    stack_free(&stack);

    return graphs;
}

static void dfs_visit_help(vertex_t *vertex, vertex_t *adjasent, stack_t *stack,
        edge_t *edge, uint32_t *count, linked_list_t *graphs);

static void dfs_visit(vertex_t *vertex, uint32_t *count, stack_t *stack,
        linked_list_t *graphs)
{
    uint32_t i;
    edge_t *edge;
    vertex_t *adjasent;

    vertex->visited = 1;
    *count = *count + 1;
    B_D(vertex) = *count;
    B_LOW(vertex) = *count;

    for (i = 0; i < vertex->outgoing_len; i++) {
        edge = &vertex->outgoing[i];
        adjasent = edge->end;

        dfs_visit_help(vertex, adjasent, stack, edge, count, graphs);
    }

    for (i = 0; i < vertex->incoming_len; i++) {
        edge = &vertex->incoming[i];
        adjasent = edge->end;

        dfs_visit_help(vertex, adjasent, stack, edge, count, graphs);
    }
}

static void dfs_visit_help(vertex_t *vertex, vertex_t *adjasent, stack_t *stack,
        edge_t *edge, uint32_t *count, linked_list_t *graphs)
{
    digraph_t *graph;

    if (!adjasent->visited) {
        stack_push(stack, edge);
        B_PARENT(adjasent) = vertex;
        dfs_visit(adjasent, count, stack, graphs);

        if (B_LOW(adjasent) >= B_D(vertex)) {
            graph = malloc(sizeof(digraph_t));
            if (graph == NULL)
                mem_err();
            *graph = output_comp(vertex, adjasent, stack);
            linked_list_add_end(graphs, graph);
        }

        B_LOW(vertex) = min(B_LOW(vertex), B_LOW(adjasent));
    } else if (B_PARENT(vertex) != adjasent && B_D(adjasent) < B_D(vertex)) {
        stack_push(stack, edge);
        B_LOW(vertex) = min(B_LOW(vertex), B_D(adjasent));
    }
}

/* Functions used in map. */
static int hash(void const *v);
static int cmp_keys(void const *v1, void const *v2);

static void output_cmp_add_edge(digraph_t *graph, edge_t const *edge,
        map_t *vertices);

static digraph_t output_comp(vertex_t *vertex1, vertex_t *vertex2,
        stack_t *stack)
{
    edge_t *edge;
    digraph_t graph;
    map_t vertices; /* From vertex_t to vertex_t. */

    graph_init(&graph);
    map_init(&vertices, 64, hash, cmp_keys);

    do {
        edge = stack_pop(stack);

        if (edge == NULL)
            break;

        output_cmp_add_edge(&graph, edge, &vertices);
    } while (edge->start != vertex1 || edge->end != vertex2);

    map_free(&vertices);

    return graph;
}

static int hash(void const *v)
{
    vertex_t const *vertex = (vertex_t const *) v;

    return (int) vertex->unique_id;
}

static int cmp_keys(void const *v1, void const *v2)
{
    vertex_t const *vertex1 = (vertex_t const *) v1;
    vertex_t const *vertex2 = (vertex_t const *) v2;

    return !(vertex1->unique_id == vertex2->unique_id);
}

static void output_cmp_add_edge(digraph_t *graph, edge_t const *edge,
        map_t *vertices)
{
    vertex_t *start = (vertex_t *) map_get(vertices, edge->start);
    vertex_t *end = (vertex_t *) map_get(vertices, edge->end);

    if (start == NULL) {
        start = new_vertex_id(edge->start->unique_id);
        graph_add_vertex_pointer(graph, start);
        map_put(vertices, edge->start, start);
    }
    if (end == NULL) {
        end = new_vertex_id(edge->end->unique_id);
        graph_add_vertex_pointer(graph, end);
        map_put(vertices, edge->end, end);
    }

    graph_add_edge_pointer(graph, start, end, edge->weight);
}

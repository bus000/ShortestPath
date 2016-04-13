#include "bicomponents.h"
#include "stack.h"
#include "graph_labeling.h"
#include <stdlib.h>

static biconnect_label_t default_label = { .parent = NULL, .d = 0 };

static void dfs_visit(vertex_t *vertex, uint32_t *count, stack_t *stack);
static void output_comp(vertex_t *vertex1, vertex_t *vertex2, stack_t *stack);
static uint32_t min(uint32_t a, uint32_t b);

int biconnect(digraph_t *graph)
{
    uint32_t count = 0, i;
    vertex_t *vertex;
    stack_t stack = stack_init(graph->vertices_len);

    graph_init_labels(graph, &default_label, sizeof(biconnect_label_t));

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];

        if (!vertex->visited)
            dfs_visit(vertex, &count, &stack);
    }

    /* Set all visited to 0. */
    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        vertex->visited = 0;
    }

    /* Free resources used by function. */
    stack_free(&stack);

    return 0;
}

static void dfs_visit_help(vertex_t *vertex, vertex_t *adjasent, stack_t *stack,
        edge_t *edge, uint32_t *count);

static void dfs_visit(vertex_t *vertex, uint32_t *count, stack_t *stack)
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

        dfs_visit_help(vertex, adjasent, stack, edge, count);
    }

    for (i = 0; i < vertex->incoming_len; i++) {
        edge = &vertex->incoming[i];
        adjasent = edge->end;

        dfs_visit_help(vertex, adjasent, stack, edge, count);
    }
}

static void dfs_visit_help(vertex_t *vertex, vertex_t *adjasent, stack_t *stack,
        edge_t *edge, uint32_t *count)
{
    if (!adjasent->visited) {
        stack_push(stack, edge);
        B_PARENT(adjasent) = vertex;
        dfs_visit(adjasent, count, stack);

        if (B_LOW(adjasent) >= B_D(vertex))
            output_comp(vertex, adjasent, stack);

        B_LOW(vertex) = min(B_LOW(vertex),
                B_LOW(adjasent));
    } else if (B_PARENT(vertex) != adjasent &&
            B_D(adjasent) < B_D(vertex)) {
        stack_push(stack, edge);
        B_LOW(vertex) = min(B_LOW(vertex),
                B_D(adjasent));
    }
}

static void output_comp(vertex_t *vertex1, vertex_t *vertex2, stack_t *stack)
{
    edge_t *edge;

    printf("new biconnected component found.\n"); fflush(stdout);

    do {
        edge = stack_pop(stack);

        if (edge == NULL)
            break;

        printf("%u -> %u\n", edge->start->unique_id, edge->end->unique_id); fflush(stdout);
    } while (edge->start == vertex1 && edge->end == vertex2);
}

static inline uint32_t min(uint32_t a, uint32_t b)
{
    return a < b ? a : b;
}

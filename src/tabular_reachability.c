#include "tabular_reachability.h"
#include "mem_man.h"
#include "error.h"
#include "stack.h"
#include <stdlib.h>

static void add_reaching_help(vertex_t const *vertex, int8_t *vertex_reaching)
{
    stack_t stack = stack_init(128); /* Stack of vertex_t *. */
    vertex_t *current, *neighbour;
    uint32_t i;

    stack_push(&stack, vertex);

    while ((current = (vertex_t *) stack_pop(&stack)) != NULL) {
        if (vertex_reaching[current->graph_index])
            continue;

        vertex_reaching[current->graph_index] = 1;

        for (i = 0; i < current->outgoing_len; i++) {
            neighbour = current->outgoing[i].end;
            stack_push(&stack, neighbour);
        }
    }

    stack_free(&stack);
}

/* Performs a depth first search of the graph recording all reachable vertices
 * to the pointer supplied. */
static int8_t * add_reaching(digraph_t const *graph, vertex_t const *vertex)
{
    int8_t *vertex_reaching;

    CALLOC(vertex_reaching, graph->vertices_len, sizeof(int8_t));

    add_reaching_help(vertex, vertex_reaching);

    return vertex_reaching;
}

table_reachability_t table_init(digraph_t const *graph)
{
    uint32_t i;
    table_reachability_t table;
    vertex_t *vertex;

    MALLOC(table.reaches, sizeof(int8_t *) * graph->vertices_len);

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        table.reaches[i] = add_reaching(graph, vertex);
    }

    table.side_len = graph->vertices_len;

    return table;
}

int inline table_reaches(table_reachability_t const *table, vertex_t const *v1,
        vertex_t const *v2)
{
    return table->reaches[v1->graph_index][v2->graph_index];
}

void table_free(table_reachability_t *table)
{
    uint32_t i;

    for (i = 0; i < table->side_len; i++)
        FREE(table->reaches[i]);

    FREE(table->reaches);
}
#include "tabular_reachability.h"
#include "error.h"
#include <stdlib.h>

static void add_reaching_help(vertex_t const *vertex, int8_t *vertex_reaching)
{
    uint32_t i;
    vertex_t const *outgoing;

    if (vertex_reaching[vertex->graph_index])
        return;

    vertex_reaching[vertex->graph_index] = 1;

    for (i = 0; i < vertex->outgoing_len; i++) {
        outgoing = vertex->outgoing[i].end;
        add_reaching_help(outgoing, vertex_reaching);
    }
}

/* Performs a depth first search of the graph recording all reachable vertices
 * to the pointer supplied. */
static int8_t * add_reaching(digraph_t const *graph, vertex_t const *vertex)
{
    int8_t *vertex_reaching;

    CALLOC(vertex_reaching, graph->vertices_len, sizeof(int8_t));

    add_reaching_help(vertex, vertex_reaching);

    vertex_reaching[vertex->graph_index] = 1; /* A vertex reaches itself. */

    return vertex_reaching;
}

table_reachability_t table_init(digraph_t const *graph)
{
    uint32_t i;
    table_reachability_t table;

    MALLOC(table.reaches, sizeof(int8_t *) * graph->vertices_len);

    for (i = 0; i < graph->vertices_len; i++) {
        table.reaches[i] = add_reaching(graph, graph->vertices[i]);
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
        free(table->reaches[i]);

    free(table->reaches);
}

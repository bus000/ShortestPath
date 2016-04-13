#include "palm_tree.h"
#include "graph_labeling.h"

static palm_tree_label_t default_label = { .number = 0 };

static uint32_t min(uint32_t a, uint32_t b);

uint32_t n;
static void palm_tree_DFS(vertex_t *vertex1, vertex_t *vertex2)
{
    uint32_t i;
    palm_tree_label_t *v1_label = (palm_tree_label_t *) vertex1->label;
    vertex_t *adjasent;
    palm_tree_label_t *adjasent_label;

    n += 1;
    v1_label->number = n;
    v1_label->lowpt1 = n;
    v1_label->lowpt2 = n;

    for (i = 0; i < vertex1->outgoing_len; i++) {
        adjasent = vertex1->outgoing[i].end;
        adjasent_label = (palm_tree_label_t *) adjasent->label;

        if (adjasent_label->number == 0) {
            /* Mark vertex1, adjasent as a tree arc. */
            palm_tree_DFS(adjasent, vertex1);

            if (adjasent_label->lowpt1 < v1_label->lowpt1) {
                v1_label->lowpt2 = min(v1_label->lowpt1, adjasent_label->lowpt2);
                v1_label->lowpt1 = adjasent_label->lowpt1;
            } else if (adjasent_label->lowpt1 == v1_label->lowpt1) {
                v1_label->lowpt2 = min(v1_label->lowpt2, adjasent_label->lowpt2);
            } else {
                v1_label->lowpt2 = min(v1_label->lowpt2, adjasent_label->lowpt1);
            }
        } else if (adjasent_label->number < v1_label->number &&
                adjasent != vertex2) {
            /* Mark vertex1, adjasent as a frond. */

            if (adjasent_label->number < v1_label->lowpt1) {

            }
        }
    }

    for (i = 0; i < vertex1->incoming_len; i++) {
        adjasent = vertex1->incoming[i].end;
        adjasent_label = (palm_tree_label_t *) adjasent->label;

        if (adjasent_label->number == 0) {
            /* Mark vertex1, adjasent as a tree arc. */
            palm_tree_DFS(adjasent, vertex1);
        } else if (adjasent_label->number < v1_label->number &&
                adjasent != vertex2) {
            /* Mark vertex1, adjasent as a frond. */
        }
    }
}

void palm_tree(digraph_t *graph)
{
    if (graph->vertices_len == 0)
        return;

    n = 0;
    graph_init_labels(graph, &default_label, sizeof(palm_tree_label_t));

    palm_tree_DFS(graph->vertices[0], NULL);
}

int inline palm_tree_arc(digraph_t const *graph, edge_t const *edge)
{
    palm_tree_label_t *v1_label = (palm_tree_label_t *) edge->start->label;
    palm_tree_label_t *v2_label = (palm_tree_label_t *) edge->end->label;

    return v1_label->number < v2_label->number;
}

int inline palm_tree_frond(digraph_t const *graph, edge_t const *edge)
{
    palm_tree_label_t *v1_label = (palm_tree_label_t *) edge->start->label;
    palm_tree_label_t *v2_label = (palm_tree_label_t *) edge->end->label;

    return v2_label->number < v1_label->number;
}

static inline uint32_t min(uint32_t a, uint32_t b)
{
    return a < b ? a : b;
}

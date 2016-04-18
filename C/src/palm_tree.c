#include "palm_tree.h"
#include "graph_labeling.h"
#include "util.h"
#include "set.h"
#include <stdlib.h>

static palm_tree_label_t default_label = { .number = -1, .l = -1, .ll = -1};

static int edge_hash(void const *e);
static int edge_cmp(void const *e1, void const *e2);

static inline int is_frond(vertex_t const *u, vertex_t const *v,
        vertex_t const *w)
{
    return u != NULL &&
        w->unique_id != u->unique_id &&
        0 <= PALM_NUMBER(w) &&
        PALM_NUMBER(w) < PALM_NUMBER(v);
}

static inline int already_directed(vertex_t const *u, vertex_t const *v,
        vertex_t const *w)
{
    return (u != NULL && w->unique_id == u->unique_id) ||
        PALM_NUMBER(v) < PALM_NUMBER(w);
}

static void palm_tree_DFS(vertex_t const *u, vertex_t *v, uint32_t *n)
{
    uint32_t i;
    vertex_t *w;

    PALM_NUMBER(v) = *n;
    *n = *n + 1;

    PALM_L(v) = PALM_NUMBER(v);
    PALM_LL(v) = PALM_NUMBER(v);

    /* Graph is considered undirected, so loop through both outgoing and
     * incoming edges. */
    for (i = 0; i < v->outgoing_len; i++) {
        w = v->outgoing[i].end;

        if (is_frond(u, v, w)) {
            if (PALM_NUMBER(w) < PALM_L(v)) {
                PALM_LL(v) = PALM_L(v);
                PALM_L(v) = PALM_NUMBER(w);
            } else if (PALM_NUMBER(w) > PALM_L(v)) {
                PALM_LL(v) = min(PALM_LL(v), PALM_NUMBER(w));
            }
        } else if (already_directed(u, v, w)) {
            vertex_remove_outgoing(v, w);
            /* Since edge is removed, don't increment i. */
            i -= 1;
        } else if (PALM_NUMBER(w) == -1) {
            palm_tree_DFS(v, w, n);

            if (PALM_L(w) < PALM_L(v) && PALM_L(w) < PALM_LL(w)) {
                PALM_LL(v) = min(PALM_L(v), PALM_LL(w));
                PALM_L(v) = PALM_L(w);
            } else if (PALM_L(w) < PALM_L(v) && PALM_L(w) == PALM_LL(w)) {
                PALM_LL(v) = PALM_L(v);
                PALM_L(v) = PALM_L(w);
            } else if (PALM_L(w) == PALM_L(v) && PALM_L(w) < PALM_LL(w)) {
                PALM_LL(v) = min(PALM_LL(v), PALM_LL(w));
            } else if (PALM_L(w) > PALM_L(v)) {
                PALM_LL(v) = min(PALM_LL(v), PALM_L(w));
            }
        }
    }

    for (i = 0; i < v->incoming_len; i++) {
        w = v->incoming[i].end;

        if (is_frond(u, v, w)) {
            if (PALM_NUMBER(w) < PALM_L(v)) {
                PALM_LL(v) = PALM_L(v);
                PALM_L(v) = PALM_NUMBER(w);
            } else if (PALM_NUMBER(w) > PALM_L(v)) {
                PALM_LL(v) = min(PALM_LL(v), PALM_NUMBER(w));
            }
        } else if (already_directed(u, v, w)) {
            vertex_remove_incoming(v, w);
            /* Since edge is removed, don't increment i. */
            i -= 1;
        } else if (PALM_NUMBER(w) == -1) {
            palm_tree_DFS(v, w, n);

            if (PALM_L(w) < PALM_L(v) && PALM_L(w) < PALM_LL(w)) {
                PALM_LL(v) = min(PALM_L(v), PALM_LL(w));
                PALM_L(v) = PALM_L(w);
            } else if (PALM_L(w) < PALM_L(v) && PALM_L(w) == PALM_LL(w)) {
                PALM_LL(v) = PALM_L(v);
                PALM_L(v) = PALM_L(w);
            } else if (PALM_L(w) == PALM_L(v) && PALM_L(w) < PALM_LL(w)) {
                PALM_LL(v) = min(PALM_LL(v), PALM_LL(w));
            } else if (PALM_L(w) > PALM_L(v)) {
                PALM_LL(v) = min(PALM_LL(v), PALM_L(w));
            }
        }
    }
}

static void sort_adjasent(digraph_t *graph)
{
    uint32_t i, buckets = graph->vertices_len * 2;
    set_t *bucket = malloc(sizeof(set_t) * buckets);

    for (i = 0; i < buckets; i++)
        bucket[i] = set_init(edge_hash, edge_cmp);

    /* Free resources used in the function. */
    for (i = 0; i < buckets; i++)
        set_free(&bucket[i]);
}

int palm_tree(digraph_t *graph)
{
    uint32_t n = 1;
    vertex_t *first_vertex;

    if (graph->vertices_len == 0)
        return -1;

    graph_init_labels(graph, &default_label, sizeof(palm_tree_label_t));

    first_vertex = graph->vertices[0];
    PALM_NUMBER(first_vertex) = 0;

    palm_tree_DFS(NULL, first_vertex, &n);
    sort_adjasent(graph);

    return 0;
}

int inline palm_tree_arc(digraph_t const *graph, edge_t const *edge)
{
    return PALM_NUMBER(edge->start) < PALM_NUMBER(edge->end);
}

int inline palm_tree_frond(digraph_t const *graph, edge_t const *edge)
{
    return PALM_NUMBER(edge->end) < PALM_NUMBER(edge->start);
}

int64_t inline palm_number(vertex_t const *vertex)
{
    return PALM_NUMBER(vertex);
}

static int edge_hash(void const *e)
{
    edge_t const *edge = (edge_t const *) e;

    return edge->start->unique_id + edge->end->unique_id + edge->weight;
}

static int edge_cmp(void const *e1, void const *e2)
{
    edge_t const *edge1 = (edge_t const *) e1;
    edge_t const *edge2 = (edge_t const *) e2;

    return edge1->start->unique_id == edge2->start->unique_id &&
        edge1->end->unique_id == edge2->end->unique_id &&
        edge1->weight == edge2->weight;
}

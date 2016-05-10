#include "palm_tree.h"
#include "graph_labeling.h"
#include "util.h"
#include "set.h"
#include <stdlib.h>

static palm_tree_label_t default_label = { .number = -1, .l = -1, .ll = -1 };

static int edge_hash(void const *e)
{
    edge_t const *edge = (edge_t const *) e;

    return edge->start->unique_id + edge->end->unique_id + edge->weight;
}

static int edge_cmp(void const *e1, void const *e2)
{
    edge_t const *edge1 = (edge_t const *) e1;
    edge_t const *edge2 = (edge_t const *) e2;

    if (edge1->start->unique_id == edge2->start->unique_id &&
        edge1->end->unique_id == edge2->end->unique_id &&
        edge1->weight == edge2->weight)
        return 0;
    else if (edge1->start->unique_id == edge2->end->unique_id &&
            edge1->end->unique_id == edge2->start->unique_id &&
            edge1->weight == edge2->weight)
        return 0;
    else
        return 1;
}

static uint32_t edge_score(edge_t const *edge)
{
    if (palm_tree_frond(edge))
        return 2 * palm_number(edge->end);
    else if (palm_tree_arc(edge) &&
            palm_second_lowest(edge->end) == palm_number(edge->end))
        return 2 * palm_lowest(edge->end);
    else if (palm_tree_arc(edge) &&
            palm_second_lowest(edge->end) < palm_number(edge->end))
        return 2 * palm_lowest(edge->end) + 1;


    fprintf(stderr, "Error should never not hit any of the cases above\n");
    return -1;
}

static inline int is_frond(vertex_t const *u, vertex_t const *v,
        vertex_t const *w)
{
    return w->unique_id != u->unique_id &&
        0 <= PALM_NUMBER(w) &&
        PALM_NUMBER(w) < PALM_NUMBER(v);
}

static inline int already_directed(vertex_t const *u, vertex_t const *v,
        vertex_t const *w)
{
    return w->unique_id == u->unique_id || PALM_NUMBER(v) < PALM_NUMBER(w);
}

static void palm_tree_DFS(vertex_t const *u, vertex_t *v, uint32_t *n)
{
    uint32_t i;
    vertex_t *w;
    int64_t tmp;

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
                tmp = PALM_L(v);
                PALM_L(v) = PALM_NUMBER(w);
                PALM_LL(v) = tmp;
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
            } else if (PALM_L(w) == PALM_L(v) && PALM_L(w) == PALM_LL(w)) {
                /* Skip. */
            } else if (PALM_L(w) > PALM_L(v)) {
                PALM_LL(v) = min(PALM_LL(v), PALM_L(w));
            }
        }
    }

    for (i = 0; i < v->incoming_len; i++) {
        w = v->incoming[i].end;

        if (is_frond(u, v, w)) {
            if (PALM_NUMBER(w) < PALM_L(v)) {
                tmp = PALM_L(v);
                PALM_L(v) = PALM_NUMBER(w);
                PALM_LL(v) = tmp;
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
            } else if (PALM_L(w) == PALM_L(v) && PALM_L(w) == PALM_LL(w)) {
                /* Skip. */
            } else if (PALM_L(w) > PALM_L(v)) {
                PALM_LL(v) = min(PALM_LL(v), PALM_L(w));
            }
        }
    }
}

static void sort_adjasent(digraph_t *graph)
{
    uint32_t i, j, buckets = graph->vertices_len * 2;
    set_t *bucket;
    vertex_t *vertex;
    edge_t const *edge;
    edge_t *edges_back, *edges_end;
    actual_list_t *edges;

    MALLOC(edges_back, sizeof(edge_t) * graph->edges_len);
    MALLOC(bucket, sizeof(set_t) * buckets);

    for (i = 0; i < buckets; i++)
        bucket[i] = set_init(edge_hash, edge_cmp);

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];

        for (j = 0; j < vertex->outgoing_len; j++) {
            *edges_end = vertex->outgoing[j];

            set_add(&bucket[edge_score(edges_end)], edges_end);
            edges_end += 1;
        }

        for (j = 0; j < vertex->incoming_len; j++) {
            *edges_end = vertex->incoming[j];

            set_add(&bucket[edge_score(edges_end)], edges_end);
            edges_end += 1;
        }

        vertex->outgoing_len = 0;
        vertex->incoming_len = 0;
    }

    for (i = 0; i < buckets; i++) {
        for (edges = set_get_contents(&bucket[i]).start; edges != NULL;
                edges = edges->next) {
            edge = edges->element;

            graph_add_edge_pointer(graph, edge->start, edge->end, edge->weight);
        }
    }

    /* Free resources used in the function. */
    for (i = 0; i < buckets; i++)
        set_free(&bucket[i]);

    free(bucket);
    free(edges_back);
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
    PALM_L(first_vertex) = 0;
    PALM_LL(first_vertex) = 0;

    if (first_vertex->outgoing_len != 0) {
        palm_tree_DFS(first_vertex, first_vertex->outgoing[0].end, &n);
        first_vertex->outgoing_len = 1;
        first_vertex->incoming_len = 0;
    } else if (first_vertex->incoming_len != 0) {
        palm_tree_DFS(first_vertex, first_vertex->incoming[0].end, &n);
        first_vertex->incoming_len = 1;
    } else {
        fprintf(stderr,
                "Error: Cannot make palm tree of graph with only one vertex.\n");
        return -1;
    }

    sort_adjasent(graph);

    return 0;
}

int inline palm_tree_arc(edge_t const *edge)
{
    return PALM_NUMBER(edge->start) < PALM_NUMBER(edge->end);
}

int inline palm_tree_frond(edge_t const *edge)
{
    return PALM_NUMBER(edge->end) < PALM_NUMBER(edge->start);
}

int64_t inline palm_number(vertex_t const *vertex)
{
    return PALM_NUMBER(vertex);
}

int64_t inline palm_lowest(vertex_t const *vertex)
{
    return PALM_L(vertex);
}

int64_t inline palm_second_lowest(vertex_t const *vertex)
{
    return PALM_LL(vertex);
}

vertex_t * palm_find(digraph_t const *graph, int64_t pnumber)
{
    uint32_t i;
    vertex_t *vertex;

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        if (palm_number(vertex) == pnumber)
            return vertex;
    }

    return NULL;
}

void print_palm(digraph_t *graph)
{
    uint32_t i, j;
    vertex_t *vertex, *adjasent;
    edge_t edge;

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];

        printf("vertex %u has palm number %" PRIu64 "\n", vertex->unique_id,
                palm_number(vertex));

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

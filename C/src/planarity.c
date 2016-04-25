#include "planarity.h"
#include "linked_list.h"
#include "graph.h"
#include "bicomponents.h"
#include "palm_tree.h"
#include <stdlib.h>

/* Returns the spine of the given vertex. */
static linked_list_t spine(vertex_t const *vertex, vertex_t const *u);

/* Runs the planarity algorithm of a graph, assumes the graph is in palm tree
 * form and is biconnected. */
static int planarity(vertex_t *cstart, vertex_t *u, vertex_t *v, int *x,
        linked_list_t *b, uint32_t *n_side);

static int lace(linked_list_t const *t, vertex_t const *w);

static void merge(linked_list_t *b);

int planar(digraph_t *graph)
{
    linked_list_t bicomponents = biconnect(graph), b;
    digraph_t *biconnected;
    actual_list_t *list;
    int x;
    uint32_t n_side;


    for (list = bicomponents.start; list != NULL; list = list->next) {
        biconnected = (digraph_t *) list->element;
        palm_tree(biconnected);

        /*side = malloc(sizeof(int) * (biconnected->edges_len -*/
                /*biconnected->vertices_len));*/
        n_side = 0;
        x = 1;
        linked_list_init(&b);
        planarity(biconnected->vertices[0], biconnected->vertices[0],
                biconnected->vertices[0]->outgoing[0].end, &x, &b, &n_side);
    }

    /* TODO: Collect all the planar graphs created by planarity and embed them
     * all on a sphere. */

    /* TODO: Free resources used in the function. */

    return 0;
}

static int planarity(vertex_t *cstart, vertex_t *u, vertex_t *v, int *x,
        linked_list_t *b, uint32_t *n_side)
{
    block_t inside, outside, tmp;
    linked_list_t spi = spine(v, u);
    vertex_t *w = (vertex_t *) spi.end->element;
    blocks_t *new_block = malloc(sizeof(blocks_t)), *blocks;
    int lace1, lace2;

    /* Add s to SP. */
    linked_list_init(&inside.a);
    linked_list_init(&outside.a);
    linked_list_singular_int(&inside.s, *n_side);
    linked_list_init(&outside.s);

    new_block->i.a = inside.a;
    new_block->i.s = inside.s;
    new_block->o.a = outside.a;
    new_block->o.s = outside.s;

    if (palm_number(w) > palm_number(cstart)) {
        linked_list_add_end(&inside.a, w);
    }

    *n_side = *n_side + 1;
    linked_list_add_end(b, new_block);

    while (*x && b->len > 1) {
        blocks = (blocks_t *) linked_list_get(b, -2);
        lace1 = lace(&(blocks->i.a), w);
        lace2 = lace(&(blocks->o.a), w);

        if (lace1 && lace2) {
            *x = 0;
        } else if (!lace1 && lace2) {
            merge(b);
        } else if (lace1 && !lace2) {
            blocks = (blocks_t *) linked_list_get(b, -2);
            tmp = blocks->i;
            blocks->i = blocks->o;
            blocks->o = tmp;

            merge(b);
        } else {
            break;
        }
    }

    return 0;
}

static linked_list_t spine(vertex_t const *vertex, vertex_t const *u)
{
    linked_list_t list;

    linked_list_init(&list);

    while (palm_number(vertex) > palm_number(u)) {
        linked_list_add_end(&list, vertex);
        vertex = vertex->outgoing[0].end;
    }

    return list;
}

static int lace(linked_list_t const *t, vertex_t const *w)
{
    vertex_t const *vertex;

    if (t->len == 0)
        return 0;

    vertex = linked_list_get(t, -1);

    return palm_number(vertex) > palm_number(w);
}

static void merge(linked_list_t *b)
{
    blocks_t *blocks1 = (blocks_t *) linked_list_get(b, -1);
    blocks_t *blocks2 = (blocks_t *) linked_list_get(b, -2);
    linked_list_t *blocks1a = &(blocks1->i.a);
    linked_list_t *blocks2a = &(blocks2->i.a);

    linked_list_concat(blocks2a, blocks1a);
}

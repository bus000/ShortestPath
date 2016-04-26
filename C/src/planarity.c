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
static void planarity(vertex_t *cstart, vertex_t *u, vertex_t *v, uint32_t *x,
        linked_list_t *b, uint32_t *n_side);

static int lace(linked_list_t const *t, vertex_t const *w);

static void merge(linked_list_t *b);

static void purge(linked_list_t *q, vertex_t *y);

static void fixside(linked_list_t *vertices, int inside);

static void delete_from(linked_list_t *vertices, vertex_t *y);

static uint32_t make_o_empty(linked_list_t *q);

uint32_t planar(digraph_t *graph)
{
    linked_list_t bicomponents = biconnect(graph), b; /* b is list of blocks_t. */
    digraph_t *biconnected;
    actual_list_t *list;
    uint32_t x;
    uint32_t n_side;

    for (list = bicomponents.start; list != NULL; list = list->next) {
        biconnected = (digraph_t *) list->element;
        palm_tree(biconnected);

        n_side = 0;
        x = 1;
        b = linked_list_init();
        planarity(biconnected->vertices[0], biconnected->vertices[0],
                biconnected->vertices[0]->outgoing[0].end, &x, &b, &n_side);

        if (!x)
            return x;
    }

    /* TODO: Collect all the planar graphs created by planarity and embed them
     * all on a sphere. */

    /* TODO: Free resources used in the function. */

    return x;
}

static void planarity(vertex_t *cstart, vertex_t *u, vertex_t *v, uint32_t *x,
        linked_list_t *b, uint32_t *n_side)
{
    block_t inside, outside, tmp;
    linked_list_t spi = spine(v, u); /* List of vertices. */
    vertex_t *w = (vertex_t *) spi.end->element;
    blocks_t *new_block = malloc(sizeof(blocks_t)), *blocks;
    int lace1, lace2;

    /* Add s to SP. */
    inside.a = linked_list_init();
    outside.a = linked_list_init();
    inside.s = linked_list_singular_int(*n_side);
    outside.s = linked_list_init();

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

    /* Build bipartite partition QP for the segments. */
    linked_list_t q = linked_list_init(); /* List of blocks_t. */
    uint32_t k;

    while (*x && spi.len != 0) {
        vertex_t *y = (vertex_t *) linked_list_get(&spi, -1);
        linked_list_remove_last(&spi);
        purge(&q, y);

        for (k = 0; *x && k < y->outgoing_len; k++)
            planarity(w, y, y->outgoing[k].end, x, &q, n_side);
    }

    /* Check theorem 2.4 part b and add attachments to b. */
    purge(&q, u);
    linked_list_t t = linked_list_init(); /* List of vertices. */

    while (*x && q.len != 0) {
        blocks_t *last = (blocks_t *) linked_list_get(&q, -1);
        *x = make_o_empty(&q);
        fixside(&last->i.s, 1);
        fixside(&last->o.s, 0);

        linked_list_prepend(&t, &last->i.a);
        linked_list_remove_last(&q);
    }

    blocks_t *last = (blocks_t *) linked_list_get(b, -1);
    linked_list_concat(&last->i.a, &t);
}

static linked_list_t spine(vertex_t const *vertex, vertex_t const *u)
{
    linked_list_t list = linked_list_init();

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
    block_t *blocks1i = &blocks1->i;
    block_t *blocks2i = &blocks2->i;
    block_t *blocks1o = &blocks1->o;
    block_t *blocks2o = &blocks2->o;

    linked_list_concat(&blocks2i->a, &blocks1i->a);
    linked_list_concat(&blocks2o->a, &blocks1o->a);
    linked_list_concat(&blocks2i->s, &blocks1i->s);
    linked_list_concat(&blocks2o->s, &blocks1o->s);
}

/* q is list of blocks_t pointers. */
static void purge(linked_list_t *q, vertex_t *y)
{
    blocks_t *last;
    linked_list_t *inside_a, *outside_a, *inside_s, *outside_s;

    while (q->len != 0) {
        last = (blocks_t *) linked_list_get(q, -1);
        inside_a = &(last->i.a);
        outside_a = &(last->o.a);
        inside_s = &(last->i.s);
        outside_s = &(last->o.s);

        delete_from(inside_a, y);
        delete_from(outside_a, y);

        if (inside_a->len == 0 && outside_a->len == 0) {
            fixside(inside_s, 1);
            fixside(outside_s, 0);
            linked_list_remove_last(q);
        } else {
            break;
        }
    }
}

/* vertices is a list of vertex_t *. */
static void fixside(linked_list_t *vertices, int inside)
{
    vertex_t *last;

    while (vertices->len != 0) {
        last = (vertex_t *) linked_list_get(vertices, -1);
        PALM_SIDE(last) = inside;
        linked_list_remove_last(vertices);
    }
}

/* vertices is a list of vertex_t *. */
static void delete_from(linked_list_t *vertices, vertex_t *y)
{
    vertex_t *last = (vertex_t *) linked_list_get(vertices, -1);

    while (vertices->len != 0 && palm_number(last) >= palm_number(y)) {
        linked_list_remove_last(vertices);
        last = (vertex_t *) linked_list_get(vertices, -1);
    }
}

/* q is list of blocks_t. */
static uint32_t make_o_empty(linked_list_t *q)
{
    blocks_t *last = (blocks_t *) linked_list_get(q, -1);
    block_t tmp;

    if (last->i.a.len != 0 && last->o.a.len != 0) {
        return 0;
    } else if (last->i.a.len > 0 && last->o.a.len == 0) {
        tmp = last->i;
        last->i = last->o;
        last->o = tmp;

        return 1;
    }

    return 1;
}

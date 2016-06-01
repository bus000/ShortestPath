#include "planarity.h"
#include "linked_list.h"
#include "graph.h"
#include "bicomponents.h"
#include "palm_tree.h"
#include <stdlib.h>

int *side; /* TODO: Don't use global variables, but give as argument. */

blocks_t *blocks[10000] = { NULL };
blocks_t **next_block = blocks;

/* t is a list of uint32_t. */
static void fixside(linked_list_t *t, int inside)
{
    uint32_t last;

    while (t->len != 0) {
        linked_list_get_int(&last, t, -1);
        side[last] = inside;
        linked_list_remove_last(t);
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

/* Returns the spine of the given vertex. */
static linked_list_t spine(vertex_t **w, vertex_t const *u)
{
    linked_list_t spi = linked_list_init();

    while (palm_number(*w) > palm_number(u)) {
        linked_list_add_end(&spi, *w);
        *w = (*w)->outgoing[0].end;
    }

    return spi;
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

    linked_list_remove_last(b);
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

/* q is list of blocks_t. */
static int make_o_empty(linked_list_t *q)
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

/* b is list of blocks_t, TODO: name blocks. */
static void add_s_to_sp(linked_list_t *b, vertex_t *cstart, vertex_t *w,
        uint32_t *n_side, int *x)
{
    blocks_t *v = malloc(sizeof(blocks_t)); /* TODO: name new_block. */
    *next_block = v;
    next_block += 1;
    blocks_t *blocks;
    block_t tmp;
    int lace1, lace2;

    v->i.a = linked_list_init();
    v->o.a = linked_list_init();
    v->i.s = linked_list_singular_int(*n_side);
    v->o.s = linked_list_init();

    if (palm_number(w) > palm_number(cstart))
        linked_list_add_end(&v->i.a, w);
    /*else if (palm_number(w) == palm_number(cstart))*/
        /*[> TODO: verify that this is correct. <]*/
        /*return; [> translation of skip command. <]*/

    *n_side = *n_side + 1;
    linked_list_add_end(b, v);

    while (*x && b->len > 1) {
        blocks = (blocks_t *) linked_list_get(b, -2);
        lace1 = lace(&(blocks->i.a), w);
        lace2 = lace(&(blocks->o.a), w);

        if (lace1 && lace2) {
            *x = 0;
        } else if (!lace1 && lace2) {
            merge(b);
        } else if (lace1 && !lace2) {
            tmp = blocks->i;
            blocks->i = blocks->o;
            blocks->o = tmp;

            merge(b);
        } else {
            return;
        }
    }
}

static void planarity(vertex_t *cstart, vertex_t *u, vertex_t *v, int *x,
        linked_list_t *b, uint32_t *n_side);

/* spi - linked_list of vertices. */
static linked_list_t bipartite_partition(int *x, linked_list_t *spi,
        uint32_t *n_side, vertex_t *w)
{
    linked_list_t q = linked_list_init(); /* List of blocks_t. */
    uint32_t k;
    vertex_t *y;

    while (*x && spi->len != 0) {
        y = (vertex_t *) linked_list_get(spi, -1);
        linked_list_remove_last(spi);
        purge(&q, y);

        for (k = 1; *x && k < y->outgoing_len; k++)
            planarity(w, y, y->outgoing[k].end, x, &q, n_side);
    }

    return q;
}

/* q is a list of blocks_t *. */
/* b is a list of blocks_t *. */
static void add_attachments(linked_list_t *q, linked_list_t *b, vertex_t *u,
        int *x)
{
    linked_list_t t = linked_list_init(); /* List of vertex_t *. */
    blocks_t *last;

    purge(q, u);

    while (*x && q->len != 0) {
        *x = make_o_empty(q);

        last = (blocks_t *) linked_list_get(q, -1);
        fixside(&last->i.s, 1);
        fixside(&last->o.s, 0);

        linked_list_prepend(&t, &last->i.a);
        linked_list_remove_last(q);
    }

    last = (blocks_t *) linked_list_get(b, -1);
    linked_list_concat(&last->i.a, &t);

    /* Free resources used in the function. */
    linked_list_free(&t);
}

/* Runs the planarity algorithm of a graph, assumes the graph is in palm tree
 * form and is biconnected. */
/* b is list of blocks_t *, TODO: name blocks. */
static void planarity(vertex_t *cstart, vertex_t *u, vertex_t *v, int *x,
        linked_list_t *b, uint32_t *n_side)
{
    vertex_t *w = v;
    linked_list_t spi = spine(&w, u); /* List of vertices. */
    linked_list_t q;

    add_s_to_sp(b, cstart, w, n_side, x);

    q = bipartite_partition(x, &spi, n_side, w); /* List of blocks_t *. */

    add_attachments(&q, b, u, x);
}

int planar(digraph_t *graph)
{
    linked_list_t bicomponents = biconnect(graph), b; /* b is list of blocks_t. */
    digraph_t *biconnected;
    actual_list_t *list;
    int x;
    uint32_t n_side;
    vertex_t *palm0;

    printf("number of bicomponents %" PRIu64 "\n", bicomponents.len);

    for (list = bicomponents.start; list != NULL; list = list->next) {
        biconnected = (digraph_t *) list->element;

        if (palm_tree(biconnected) != 0)
            return -2;

        side = malloc(sizeof(int) *
                (biconnected->edges_len - biconnected->vertices_len));

        /* Find vertex with palm number 0. */
        palm0 = palm_find(biconnected, 0);
        if (palm0 == NULL)
            return -1;

        n_side = 0;
        x = 1;
        b = linked_list_init();

        if (palm0->outgoing[0].end == NULL)
            printf("Graph consist of a single vertex.\n");
        else
            planarity(palm0, palm0, palm0->outgoing[0].end, &x, &b, &n_side);
    }

    /* TODO: Collect all the planar graphs created by planarity and embed them
     * all on a sphere. */

    /* TODO: Free resources used in the function. */

    return 0;
}

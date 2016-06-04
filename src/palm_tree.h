#ifndef PALM_TREE_H
#define PALM_TREE_H

#include "graph.h"

typedef struct palm_tree_label_s {
    /* ID given to the palm vertex. */
    int64_t number;

    /* Lowest palm number reachable from this vertex. */
    int64_t l;

    /* Second lowest palm number reachable from this vertex. */
    int64_t ll;
} palm_tree_label_t;

#define PALM_NUMBER(vertex) (((palm_tree_label_t *) vertex->label)->number)
#define PALM_L(vertex) (((palm_tree_label_t *) vertex->label)->l)
#define PALM_LL(vertex) (((palm_tree_label_t *) vertex->label)->ll)
#define PALM_SIDE(vertex) (((palm_tree_label_t *) vertex->label)->side)

/* Create a palm tree from a graph, the graph given should be biconnected. The
 * function returns 0 if the palm tree is successfully constructed, and -1 if
 * something goes wrong. */
int palm_tree(digraph_t *graph);

/* Return true if the edge is an arc in the tree and false otherwise. Function
 * assumes that the labels of the graph are palm_tree_label_t's,
 * palm_tree(graph) should therefore be called first. */
int palm_tree_arc(edge_t const *edge);

/* Return true if the edge is a frond in the tree and false otherwise. Function
 * assumes that the labels of the graph are palm_tree_label_t's,
 * palm_tree(graph) should therefore be called first. */
int palm_tree_frond(edge_t const *edge);

/* Returns the palm number of a given vertex. The function has the same effect
 * as the macro of the same name but is a bit more type safe. */
int64_t palm_number(vertex_t const *vertex);

/* Returns the lowest palm number reachable from the vertex given. */
int64_t palm_lowest(vertex_t const *vertex);

/* Returns the second lowest palm number reachable from the vertex given. */
int64_t palm_second_lowest(vertex_t const *vertex);

/* Takes a graph that has been made to a palm tree and return the vertex with
 * the palm number given. */
vertex_t * palm_find(digraph_t const *graph, int64_t pnumber);

/* Print a palm tree. */
void print_palm(digraph_t *graph);

#endif
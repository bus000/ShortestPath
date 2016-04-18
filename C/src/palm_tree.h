#ifndef PALM_TREE_H
#define PALM_TREE_H

#include "graph.h"

typedef struct {
    /* ID given to the palm vertex. */
    int64_t number;
} palm_tree_label_t;

#define PALM_NUMBER(vertex) (((palm_tree_label_t *) vertex->label)->number)

/* Create a palm tree from a graph, the graph given should be biconnected. The
 * function returns 0 if the palm tree is successfully constructed, and -1 if
 * something goes wrong. */
int palm_tree(digraph_t *graph);

/* Return true if the edge is an arc in the tree and false otherwise. Function
 * assumes that the labels of the graph are palm_tree_label_t's,
 * palm_tree(graph) should therefore be called first. */
int palm_tree_arc(digraph_t const *graph, edge_t const *edge);

/* Return true if the edge is a frond in the tree and false otherwise. Function
 * assumes that the labels of the graph are palm_tree_label_t's,
 * palm_tree(graph) should therefore be called first. */
int palm_tree_frond(digraph_t const *graph, edge_t const *edge);

int64_t palm_number(vertex_t const *vertex);

#endif

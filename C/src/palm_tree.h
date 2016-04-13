#ifndef PALM_TREE_H
#define PALM_TREE_H

#include "graph.h"

typedef struct {
    /* ID given to the palm vertex. */
    uint32_t number;

    /* The lowest in the palm tree it is possible to reach from this vertex by
     * going up along the arc and going down using a frond. */
    uint32_t lowpt1;

    /* The second lowest in the palm tree it is possible to reach from this
     * vertex by going up along the arc and going down using a frond. */
    uint32_t lowpt2;
} palm_tree_label_t;

#define ARC   (1)
#define FROND (2)

/* Change the graph labels to create a palm tree. */
void palm_tree(digraph_t *graph);

/* Return true if the edge is an arc in the tree and false otherwise. Function
 * assumes that the labels of the graph are palm_tree_label_t's,
 * palm_tree(graph) should therefore be called first. */
int palm_tree_arc(digraph_t const *graph, edge_t const *edge);

/* Return true if the edge is a frond in the tree and false otherwise. Function
 * assumes that the labels of the graph are palm_tree_label_t's,
 * palm_tree(graph) should therefore be called first. */
int palm_tree_frond(digraph_t const *graph, edge_t const *edge);

#endif

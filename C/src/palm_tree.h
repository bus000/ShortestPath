#ifndef PALM_TREE_H
#define PALM_TREE_H

#include "graph.h"

typedef struct {
    uint32_t number;
} palm_tree_label_t;

#define ARC   (1)
#define FROND (2)

//typedef struct {
    //uint32_t type; [> Either ARC or FROND. <]
//} palm_edge_t;

//typedef struct {
    //vertex_id_t original_id; [> The ID of the vertex in the previous graph. <]
    //palm_tree_label_t label;

//} palm_tree_t;

/* Change the graph labels to create a palm tree. */
void palm_tree(graph_t *graph);

/* Return true if the edge is an arc in the tree and false otherwise. Function
 * assumes that the labels of the graph are palm_tree_label_t's,
 * palm_tree(graph) should therefore be called first. */
int palm_tree_arc(graph_t const *graph, edge_t const *edge);

/* Return true if the edge is a frond in the tree and false otherwise. Function
 * assumes that the labels of the graph are palm_tree_label_t's,
 * palm_tree(graph) should therefore be called first. */
int palm_tree_frond(graph_t const *graph, edge_t const *edge);

#endif

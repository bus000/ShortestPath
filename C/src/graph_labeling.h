#ifndef GRAPH_LABELING_H
#define GRAPH_LABELING_H

#include "graph.h"

/* Initialize all labels in the graph to be the label pointed to by label that
 * has the size, label_size. The data label is pointing to are copied and will
 * never be changed in the original version. */
void graph_init_labels(digraph_t *graph, void const *label, size_t label_size);

void graph_init_labels_size(digraph_t *graph, size_t labels_size,
        size_t label_size);

/* Set the label of the vertex vertex to the label label. The label is assumed
 * to have size graph->label_size. If this is not the case, graph_init_labels
 * should be called again with a different label size.
 *
 * The function returns -1 if the vertex does not have an initialized region
 * available, for the label. In that case, graph_init_labels should be called.
 *
 * The function returns 0 if nothing goes wrong. */
int graph_set_label(digraph_t *graph, vertex_t *vertex, void const *label);

#endif

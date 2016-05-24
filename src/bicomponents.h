#ifndef BICOMPONENTS_H
#define BICOMPONENTS_H

#include <inttypes.h>
#include "graph.h"

typedef struct biconnect_label_s {
    vertex_t *parent;
    uint32_t d;
    uint32_t low;
} biconnect_label_t;

#define B_PARENT(vertex) (((biconnect_label_t *) vertex->label)->parent)
#define B_LOW(vertex)    (((biconnect_label_t *) vertex->label)->low)
#define B_D(vertex)      (((biconnect_label_t *) vertex->label)->d)

/* Takes a connected graph and splits it in its biconnected components. The
 * biconnected components are returned as a linked list of graphs. */
linked_list_t biconnect(digraph_t *graph);

#endif

#ifndef BICOMPONENTS_H
#define BICOMPONENTS_H

#include <inttypes.h>
#include "graph.h"

typedef struct {
    vertex_t *parent;
    uint32_t d;
    uint32_t low;
} biconnect_label_t;

#define B_PARENT(vertex) (((biconnect_label_t *) vertex->label)->parent)
#define B_LOW(vertex)    (((biconnect_label_t *) vertex->label)->low)
#define B_D(vertex)      (((biconnect_label_t *) vertex->label)->d)

int biconnect(digraph_t *graph);

#endif

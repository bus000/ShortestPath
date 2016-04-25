#ifndef PLANARITY_H
#define PLANARITY_H

#include "linked_list.h"
#include "graph.h"

typedef struct {
    linked_list_t a;
    linked_list_t s;
} block_t;

typedef struct {
    block_t i; /* Inside. */
    block_t o; /* Outside. */
} blocks_t;

int planar(digraph_t *graph);

#endif

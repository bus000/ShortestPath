#ifndef PLANARITY_H
#define PLANARITY_H

#include "linked_list.h"
#include "graph.h"

typedef struct {
    linked_list_t a; /* List of vertices. */
    linked_list_t s; /* List of integers. */
} block_t;

typedef struct {
    block_t i; /* Inside. */
    block_t o; /* Outside. */
} blocks_t;

uint32_t planar(digraph_t *graph);

#endif

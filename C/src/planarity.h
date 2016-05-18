#ifndef PLANARITY_H
#define PLANARITY_H

#include "linked_list.h"
#include "graph.h"

struct planar_vertex_t;

typedef struct planar_edge_s {
    struct planar_vertex_t *start;
    struct planar_vertex_t *end;

    /* Stores the edge first encountered if moving clockwise from the edge
     * (start-end). */
    struct planar_edge_s *clockwise;

    /* Stores the edge first encountered if moving counter-clockwise from the
     * edge (start-end). */
    struct planar_edge_s *counter_clockwise;

    /* Stores the edge first encountered if moving clockwise from the edge
     * (end-start). */
    struct planar_edge_s *c_clockwise;

    /* Stores the edge first encountered if moving counter-clockwise form the
     * edge (end-start). */
    struct planar_edge_s *c_counter_clockwise;
} planar_edge_t;

typedef struct planar_vertex_s {
    vertex_id_t unique_id;

    planar_edge_t *incident_edge;
} planar_vertex_t;

typedef struct planar_digraph_s {
    /* A list of all the edges in the graph (planar_edge_t *). */
    linked_list_t edges;

    /* A list of all the vertices in the graph (planar_vertex_t *). */
    linked_list_t vertices;
} planar_digraph_t;

typedef struct block_s {
    linked_list_t a; /* List of vertices. */
    linked_list_t s; /* List of integers. */
} block_t;

typedef struct blocks_s {
    block_t i; /* Inside. */
    block_t o; /* Outside. */
} blocks_t;

extern int *side;
extern blocks_t *blocks[10000];

int planar(digraph_t *graph);

#endif

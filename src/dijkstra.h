#ifndef DIJKSTRA_H
#define DIJKSTRA_H

#include "vertex.h"
#include "graph.h"

/* A Dijkstra oracle is simply a graph with the correct labels. */
typedef struct dijkstra_oracle_s {
    vertex_t *source;
    digraph_t *graph;
} dijkstra_oracle_t;

/* Construct a reachability oracle using dijkstra. */
dijkstra_oracle_t dijkstra_init(digraph_t *graph, vertex_t *source);

/* Returns true if the source vertex of the oracle reaches the end vertex. */
int dijkstra_reaches(dijkstra_oracle_t const *oracle, vertex_t const *end);

/* Free resources used by dijkstra_oracle. */
void dijkstra_free(dijkstra_oracle_t *oracle);

#endif

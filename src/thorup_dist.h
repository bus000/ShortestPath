#ifndef THORUP_DIST
#define THORUP_DIST

#include "graph.h"

typedef struct thorup_label_s {
    uint32_t layer;
} thorup_label_t;

typedef struct reachability_oracle_s {
    /* A list of graphs, each consisting of two consecutive layers in the
     * layering algorithm. */
    linked_list_t graphs;

    /* Each graph contains a spanning tree of vertices. */
    linked_list_t spanning_trees;
} reachability_oracle_t;

/* Construct a reachability oracle by splitting the graph given into a set of
 * graphs where each dipath between two vertices can only be in two of these
 * subgraphs. Since the subgraphs are all smaller than the original graph, this
 * has made the problem smaller. */
int thorup_reach_oracle(reachability_oracle_t *oracle, digraph_t *graph);

int reachability(reachability_oracle_t const *oracle, vertex_t const *v1,
        vertex_t const *v2);

/* Returns a linked_list of linked_list's of vertices. Each of the lists
 * contains one of the separators for the graph, the length of the list will be
 * 3. */
//linked_list_t thorup_separator(planar_digraph_t const *graph);

/* Free the resources used by the reachability oracle. */
void reach_oracle_free(reachability_oracle_t *oracle);

uint32_t layering(digraph_t *graph);

#endif

#ifndef THORUP_DIST
#define THORUP_DIST

#include "graph.h"

typedef struct {
    graph_t *graphs;
    uint32_t graphs_len;
} reachability_oracle_t;

/* Construct a reachability oracle by splitting the graph given into a set of
 * graphs where each dipath between two vertices can only be in two of these
 * subgraphs. Since the subgraphs are all smaller than the original graph, this
 * has made the problem smaller. */
int thorup_reach_oracle(reachability_oracle_t *oracle, graph_t *graph);

int reachability(reachability_oracle_t const *oracle, vertex_t const *v1,
        vertex_t const *v2);

/* Free the resources used by the reachability oracle. */
void reach_oracle_free(reachability_oracle_t *oracle);

#endif

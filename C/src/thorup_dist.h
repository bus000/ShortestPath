#ifndef THORUP_DIST
#define THORUP_DIST

#include "graph.h"

typedef struct {
    graph_t *graphs;
    uint32_t graphs_len;
} reachability_oracle_t;

int thorup_reach_oracle(reachability_oracle_t *oracle, graph_t *graph);

void reach_oracle_free(reachability_oracle_t *oracle);

#endif

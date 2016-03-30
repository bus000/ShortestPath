#ifndef THORUP_DIST
#define THORUP_DIST

#include "graph.h"

typedef struct {

} reachability_oracle_t;

int thorup_reach_oracle(reachability_oracle_t *oracle, graph_t *graph);

#endif

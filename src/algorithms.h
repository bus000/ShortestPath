#ifndef ALGORITHMS_H
#define ALGORITHMS_H

#include "graph.h"

int run_dijkstra(digraph_t *graph, uint32_t tests);
int run_dijkstra_ss(digraph_t *graph, uint32_t tests);

int run_tabular(digraph_t *graph, uint32_t tests);
int run_tabular_ss(digraph_t *graph, uint32_t tests);

int run_thorup(digraph_t *graph, uint32_t tests);
int run_thorup_ss(digraph_t *graph, uint32_t tests);

int run_dfs(digraph_t *graph, uint32_t tests);
int run_dfs_ss(digraph_t *graph, uint32_t tests);

int run_layer(digraph_t *graph, uint32_t tests);
int run_layer_ss(digraph_t *graph, uint32_t tests);

int run_planar(digraph_t *graph, uint32_t tests);
int run_planar_ss(digraph_t *graph, uint32_t tests);

#endif

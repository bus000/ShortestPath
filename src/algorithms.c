#include "algorithms.h"
#include "graph.h"
#include "planarity.h"
#include "tabular_reachability.h"
#include "thorup_dist.h"
#include "dijkstra.h"
#include "util.h"
#include <time.h>
#include <stdlib.h>

static vertex_t * random_vertex(digraph_t const *graph)
{
    int i = rand_range(0, graph->vertices_len);

    return graph->vertices[i];
}

int run_dijkstra(digraph_t *graph, uint32_t tests)
{
    clock_t begin, end;
    double time_spent;
    path_t path;
    uint32_t test;
    vertex_t *v1, *v2;

    begin = clock();
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("dijkstra construction time %f\n", time_spent);

    begin = clock();
    for (test = 0; test < tests; test++) {
        v1 = random_vertex(graph);
        v2 = random_vertex(graph);
        dijkstra(&path, graph, v1, v2);
        path_free(&path);
    }
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("dijkstra querying time %f\n", time_spent);

    return 0;
}

int run_dijkstra_ss(digraph_t *graph, uint32_t tests)
{
    dijkstra_oracle_t oracle;
    clock_t begin, end;
    double time_spent;
    uint32_t test;
    vertex_t *v1, *v2;

    v1 = random_vertex(graph);

    begin = clock();
    oracle = dijkstra_init(graph, v1);
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("dijkstra ss construction time %f\n", time_spent);

    begin = clock();
    for (test = 0; test < tests; test++) {
        v2 = random_vertex(graph);
        dijkstra_reaches(&oracle, v2);
    }
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("dijkstra ss querying time %f\n", time_spent);

    return 0;
}

int run_tabular(digraph_t *graph, uint32_t tests)
{
    clock_t begin, end;
    double time_spent;
    uint32_t test;
    table_reachability_t table;
    vertex_t *v1, *v2;

    begin = clock();
    table = table_init(graph);
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("table construction time %f\n", time_spent);

    begin = clock();
    for (test = 0; test < tests; test++) {
        v1 = random_vertex(graph);
        v2 = random_vertex(graph);
        table_reaches(&table, v1, v2);
    }
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("table querying time %f\n", time_spent);

    table_free(&table);

    return 0;
}

int run_tabular_ss(digraph_t *graph, uint32_t tests)
{
    clock_t begin, end;
    double time_spent;
    uint32_t test;
    table_reachability_t table;
    vertex_t *v1, *v2;

    v1 = random_vertex(graph);

    begin = clock();
    table = table_init(graph);
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("table ss construction time %f\n", time_spent);

    begin = clock();
    for (test = 0; test < tests; test++) {
        v2 = random_vertex(graph);
        table_reaches(&table, v1, v2);
    }
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("table ss querying time %f\n", time_spent);

    table_free(&table);

    return 0;
}

int run_thorup(digraph_t *graph, uint32_t tests)
{
    return 0;
}

int run_thorup_ss(digraph_t *graph, uint32_t tests)
{
    return 0;
}

int run_dfs(digraph_t *graph, uint32_t tests)
{
    clock_t begin, end;
    double time_spent;
    uint32_t test;
    vertex_t *v1, *v2;

    begin = clock();
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("DFS construction time %f\n", time_spent);

    begin = clock();
    for (test = 0; test < tests; test++) {
        v1 = random_vertex(graph);
        v2 = random_vertex(graph);
        reach_DFS(graph, v1, v2);
    }
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("DFS querying time %f\n", time_spent);

    return 0;
}

int run_dfs_ss(digraph_t *graph, uint32_t tests)
{
    clock_t begin, end;
    double time_spent;
    uint32_t test;
    vertex_t *v1, *v2;

    v1 = random_vertex(graph);

    begin = clock();
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("DFS ss construction time %f\n", time_spent);

    begin = clock();
    for (test = 0; test < tests; test++) {
        v2 = random_vertex(graph);
        reach_DFS(graph, v1, v2);
    }
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("DFS ss querying time %f\n", time_spent);

    return 0;
}

int run_layer(digraph_t *graph, uint32_t tests)
{
    reachability_oracle_t oracle;
    clock_t begin, end;
    double time_spent;

    begin = clock();
    thorup_reach_oracle(&oracle, graph);
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
    printf("Layering construction time %f\n", time_spent);

    return 0;
}

int run_layer_ss(digraph_t *graph, uint32_t tests)
{
    return run_layer(graph, tests);
}

int run_planar(digraph_t *graph, uint32_t tests)
{
    clock_t begin, end;
    double time_spent;

    begin = clock();
    planar(graph);
    end = clock();
    time_spent = (double) (end - begin) / CLOCKS_PER_SEC;

    printf("Planarity construction time %f\n", time_spent);

    return 0;
}

int run_planar_ss(digraph_t *graph, uint32_t tests)
{
    return run_planar(graph, tests);
}

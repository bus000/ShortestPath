#include "error.h"
#include "graph.h"
#include "tabular_reachability.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef enum algorithms_e {
    ALGO_NO, ALGO_DIJKSTRA, ALGO_TABLE, ALGO_THORUP, ALGO_DFS
} algorithms_t;

typedef struct function_s {
    /* File containing the graph to read. */
    char const *graph_file;

    /* List of algorithms to run. */
    algorithms_t algorithms;

    uint32_t size;
} function_t;

static void usage(char const *program_name)
{
    printf("usage: %s [-v version] [-h help] [--dijkstra]"
            " [--thorup] [--tabular] [--DFS] [--size=value] graph_file\n"

            "   -v: display version information\n"

            "   -h: display usage information\n"

            "   --dijkstra: run the dijkstra algorithm to complete the orders\n"

            "   --thorup: run the thorup algorithm to complete the orders\n"

            "   --tabular: run the table algorithm to complete the orders\n"

            "   --DFS: run the depth first search algorithm to complete the"
                      "orders\n"

            "   --size: the size of the graph\n"

            "   graph_file: name of file containing graph data\n",

            program_name);

    exit(EXIT_SUCCESS);
}

static void version(void)
{
    printf("1.0\n");

    exit(EXIT_SUCCESS);
}

static function_t parse_args(int argc, char const *argv[])
{
    int i;
    char const *arg;
    function_t function = { .graph_file = NULL, .algorithms = ALGO_NO,
        .size = 0 };

    for (i = 1; i < argc; i++) {
        arg = argv[i];

        if (strcmp("-h", arg) == 0 || strcmp("--help", arg) == 0) {
            usage(argv[0]);
        } else if (strcmp("-v", arg) == 0 || strcmp("--version", arg) == 0) {
            version();
        } else if (strcmp("--dijkstra", arg) == 0) {
            if (function.algorithms != ALGO_NO)
                usage(argv[0]);

            function.algorithms = ALGO_DIJKSTRA;
        } else if (strcmp("--tabular", arg) == 0) {
            if (function.algorithms != ALGO_NO)
                usage(argv[0]);

            function.algorithms = ALGO_TABLE;
        } else if (strcmp("--thorup", arg) == 0) {
            if (function.algorithms != ALGO_NO)
                usage(argv[0]);

            function.algorithms = ALGO_THORUP;
        } else if (strcmp("--DFS", arg) == 0) {
            if (function.algorithms != ALGO_NO)
                usage(argv[0]);

            function.algorithms = ALGO_DFS;
        } else if (sscanf(arg, "--size=%u", &function.size) == 1) {
            /* Nop. */
        } else {
            function.graph_file = arg;
        }
    }

    return function;
}

static vertex_t * random_vertex(digraph_t const *graph)
{
    int i = rand_range(0, graph->vertices_len);

    return graph->vertices[i];
}

static int run_dijkstra(digraph_t *graph, uint32_t tests)
{
    clock_t begin, end;
    double time_spent;
    path_t path;
    uint32_t test;
    vertex_t *v1, *v2;

    begin = clock();
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("dijkstra construction time %f\n", time_spent);

    begin = clock();
    for (test = 0; test < tests; test++) {
        v1 = random_vertex(graph);
        v2 = random_vertex(graph);
        dijkstra(&path, graph, v1, v2);
        path_free(&path);
    }
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("dijkstra querying time %f\n", time_spent);

    return 0;
}

static int run_tabular(digraph_t *graph, uint32_t tests)
{
    clock_t begin, end;
    double time_spent;
    uint32_t test;
    table_reachability_t table;
    vertex_t *v1, *v2;

    begin = clock();
    table = table_init(graph);
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("table construction time %f\n", time_spent);

    begin = clock();
    for (test = 0; test < tests; test++) {
        v1 = random_vertex(graph);
        v2 = random_vertex(graph);
        table_reaches(&table, v1, v2);
    }
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("table querying time %f\n", time_spent);

    table_free(&table);

    return 0;
}

static int run_thorup(digraph_t *graph, uint32_t tests)
{
    return 0;
}

static int run_dfs(digraph_t *graph, uint32_t tests)
{

    return 0;
}

int main(int argc, char const *argv[])
{
    digraph_t graph, limited;
    function_t function = parse_args(argc, argv);

    srand(time(NULL));

    vertices_init();
    graph = read_graph(function.graph_file);

    if (function.size != 0) {
        printf("rezising to %u\n", function.size);
        limited = graph_subgraph(&graph, function.size);
        graph_free(&graph);
        graph = limited;
    }

    switch (function.algorithms) {
    case ALGO_DIJKSTRA:
        run_dijkstra(&graph, 1000);
        break;
    case ALGO_TABLE:
        run_tabular(&graph, 1000);
        break;
    case ALGO_THORUP:
        run_thorup(&graph, 1000);
        break;
    case ALGO_DFS:
        run_dfs(&graph, 1000);
        break;
    case ALGO_NO:
    default:
        usage(argv[0]);
    }

    graph_free(&graph);
    vertices_free();

    return EXIT_SUCCESS;
}

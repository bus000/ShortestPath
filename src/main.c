#include "error.h"
#include "graph.h"
#include "algorithms.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef enum algorithms_e {
    ALGO_NO, ALGO_DIJKSTRA, ALGO_TABLE, ALGO_THORUP, ALGO_DFS, ALGO_LAYER,
    ALGO_PLANAR,
} algorithms_t;

typedef struct function_s {
    /* File containing the graph to read. */
    char const *graph_file;

    /* List of algorithms to run. */
    algorithms_t algorithms;

    /* True if the algorithms should be run as single source, false
     * otherwise. */
    int8_t single_source;

    /* Generate the test graphs of different sizes from the input graph. */
    int8_t generate_graph;
} function_t;

static void usage(char const *program_name)
{
    printf("usage: %s [-v version] [-h help] [--dijkstra]"
            " [--thorup] [--tabular] [--DFS] [--layer] [--planarity]"
            " [-ss single-source] graph_file\n"

            "   -v: display version information.\n"

            "   -h: display usage information.\n"

            "   --dijkstra: run the dijkstra algorithm.\n"

            "   --thorup: run the Thorup algorithm.\n"

            "   --tabular: run the table algorithm.\n"

            "   --DFS: run the depth first search algorithm.\n"

            "   --layer: run Thorup layering.\n"

            "   --planarity: run planarity.\n"

            "   -ss: single-source, will run the algorithm with a fixed source"
            " instead of changing it on each run.\n"

            "   graph_file: name of file containing graph data\n",

            program_name);

    exit(EXIT_SUCCESS);
}

static void version(void)
{
    printf("1.0\n");

    exit(EXIT_SUCCESS);
}

__attribute__((const)) static algorithms_t algorithm(char const *arg)
{
    if (strcmp("--dijkstra", arg) == 0)
        return ALGO_DIJKSTRA;

    if (strcmp("--tabular", arg) == 0)
        return ALGO_TABLE;

    if (strcmp("--thorup", arg) == 0)
        return ALGO_THORUP;

    if (strcmp("--DFS", arg) == 0)
        return ALGO_DFS;

    if (strcmp("--layer", arg) == 0)
        return ALGO_LAYER;

    if (strcmp("--planarity", arg) == 0)
        return ALGO_PLANAR;

    return ALGO_NO;
}

static int arg_match(char const *match, char const *s, char const *l)
{
    return strcmp(match, s) == 0 || strcmp(match, l) == 0;
}

static function_t parse_args(int argc, char const *argv[])
{
    int i;
    char const *arg;
    function_t function = { .graph_file = NULL, .algorithms = ALGO_NO,
        .single_source = 0, .generate_graph = 0 };

    for (i = 1; i < argc; i++) {
        arg = argv[i];

        if (arg_match(arg, "-h", "--help")) {
            usage(argv[0]);
        } else if (arg_match(arg, "-v", "--version")) {
            version();
        } else if (algorithm(arg) != ALGO_NO) {
            if (function.algorithms != ALGO_NO)
                usage(argv[0]);
            else
                function.algorithms = algorithm(arg);
        } else if (arg_match(arg, "-gg", "--generate_graph")) {
            function.generate_graph = 1;
        } else if (arg_match(arg, "-ss", "--single-source")) {
            function.single_source = 1;
        } else {
            function.graph_file = arg;
        }
    }

    return function;
}

static void generate_graphs(digraph_t *graph)
{
    uint32_t size;
    digraph_t newgraph;
    file_t file;
    char filename[128] = { 0 };

    for (size = 500; size <= 10000; size += 500) {
        sprintf(filename, "../data/graph_%u.txt", size);
        file_init(&file, filename);
        newgraph = graph_subgraph(graph, size);

        if (newgraph.vertices_len != size) {
            fprintf(stderr, "Error graph should be size %u, but was %u\n", size,
                    newgraph.vertices_len);

            exit(EXIT_FAILURE);
        }

        graph_save(&newgraph, &file);
        file_free(&file);
        graph_free(&newgraph);
    }
}

int main(int argc, char const *argv[])
{
    digraph_t graph;
    function_t function = parse_args(argc, argv);

    srand(time(NULL));

    vertices_init();
    printf("before reading\n");
    graph = read_graph(function.graph_file);
    printf("Read graph %s of size %u\n", function.graph_file,
            graph.vertices_len);

    if (function.generate_graph) {
        generate_graphs(&graph);
        graph_free(&graph);
        vertices_free();
        return EXIT_SUCCESS;
    }

    switch (function.algorithms) {
    case ALGO_DIJKSTRA:
        if (function.single_source)
            run_dijkstra_ss(&graph, 1000);
        else
            run_dijkstra(&graph, 1000);
        break;
    case ALGO_TABLE:
        if (function.single_source)
            run_tabular_ss(&graph, 1000);
        else
            run_tabular(&graph, 1000);
        break;
    case ALGO_THORUP:
        if (function.single_source)
            run_thorup_ss(&graph, 1000);
        else
            run_thorup(&graph, 1000);
        break;
    case ALGO_DFS:
        if (function.single_source)
            run_dfs_ss(&graph, 1000);
        else
            run_dfs(&graph, 1000);
        break;
    case ALGO_LAYER:
        if (function.single_source)
            run_layer_ss(&graph, 1000);
        else
            run_layer(&graph, 1000);
        break;
    case ALGO_PLANAR:
        if (function.single_source)
            run_planar_ss(&graph, 1000);
        else
            run_planar(&graph, 1000);
        break;
    case ALGO_NO:
    default:
        usage(argv[0]);
    }

    graph_free(&graph);
    vertices_free();

    return EXIT_SUCCESS;
}

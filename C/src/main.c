#include "graph.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char const *graph_file;
} function_t;

static void usage(char const *program_name);
static void version(void);
static function_t parse_args(int argc, char const *argv[]);

int main(int argc, char const *argv[])
{
    digraph_t graph;
    function_t function = parse_args(argc, argv);

    vertices_init();

    graph = read_graph(function.graph_file);

    printf("%d\n", graph.vertices_len);

    /* Free resources used. */
    vertices_free();
    graph_free(&graph);

    return EXIT_SUCCESS;
}

static void usage(char const *program_name)
{
    printf("usage: %s [-v version] [-h help] graph_file\n"

            "   -v: get the version information\n"

            "   -h: display usage information\n"

            "   graph_file: name of file containing graph data\n",
            program_name);

    exit(EXIT_FAILURE);
}

static void version()
{
    printf("1.0\n");

    exit(EXIT_SUCCESS);
}

static function_t parse_args(int argc, char const *argv[])
{
    int i;
    char const *arg;
    function_t function = { .graph_file = NULL };

    if (argc != 2)
        usage(argv[0]);

    for (i = 1; i < argc; i++) {
        arg = argv[i];

        if (strcmp("-h", arg) == 0 || strcmp("--help", arg) == 0)
            usage(argv[0]);
        else if (strcmp("-v", arg) == 0 || strcmp("--version", arg) == 0)
            version();
        else
            function.graph_file = arg;
    }

    return function;
}

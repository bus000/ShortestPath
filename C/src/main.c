#include "file.h"
#include "error.h"
#include "graph.h"
#include "process_status.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

typedef struct {
    char const *graph_file;
} function_t;

static void usage(char const *program_name)
{
    printf("usage: %s [-v version] [-h help] graph_file\n"

            "   -v: display version information\n"

            "   -h: display usage information\n"

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

static int run_dijkstra(digraph_t *graph)
{
    path_t path;

    sleep(10);

    return dijkstra(&path, graph, graph->vertices[2]->unique_id,
            graph->vertices[3]->unique_id);
}

static int monitor(pid_t process, char const *outfilepath)
{
    memory_usage_t memstat;

    memstat_init(&memstat, process);

    while (waitpid(process, NULL, WNOHANG) == 0) {
        if (usleep(100000) == -1)
            error_code(ERR_SLEEP, "Could not sleep");

        if (memstat_update(&memstat) != 0)
            continue;

        printf("pages used %" PRIu32 "\n", memstat.program_size);
    }

    memstat_free(&memstat);

    return EXIT_SUCCESS;
}

int main(int argc, char const *argv[])
{
    pid_t pid;
    digraph_t graph;
    function_t function = parse_args(argc, argv);
    int exitcode;

    vertices_init();

    graph = read_graph(function.graph_file);

    printf("read graph\n");

    /* Fork process, child runs Dijkstra algorithm and parent monitors child's
     * memory usage and execution time. */
    if ((pid = fork()) != 0) { /* Parent. */
        exitcode = monitor(pid, "dijkstra_monitor.out");
    } else { /* Child. */
        sleep(1);
        exitcode = run_dijkstra(&graph);
        printf("child run dijkstra over.\n");
    }

    graph_free(&graph);
    vertices_free();

    return exitcode;
}

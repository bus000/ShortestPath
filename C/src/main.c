#include "file.h"
#include "error.h"
#include "graph.h"
#include "tabular_reachability.h"
#include "mem_man.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#define ALGO_DIJKSTRA (0)
#define ALGO_TABLE    (1)
#define ALGO_THORUP   (2)

/* TODO: Find vertices before running algorithm. */
typedef struct order_s {
    /* Start of path finding. */
    vertex_id_t start;

    /* End of path. */
    vertex_id_t end;

    /* If start actually reaches end. */
    int reaches;
} order_t;

typedef struct function_s {
    /* File containing the graph to read. */
    char const *graph_file;

    /* List of order_t *. These orders are the ones to be run by the algorithm
     * in order to find memory and time usage. */
    linked_list_t orders;

    /* List of algorithms to run. */
    linked_list_t algorithms;
} function_t;

static void free_functions(function_t *function)
{
    actual_list_t *list;

    for (list = function->orders.start; list != NULL; list = list->next)
        free((order_t *) list->element);

    linked_list_free(&function->orders);
    linked_list_free(&function->algorithms);
}

static void usage(char const *program_name)
{
    printf("usage: %s [-v version] [-h help] [-o order s e r] [--dijkstra] "
            "graph_file\n"

            "   -v: display version information\n"

            "   -h: display usage information\n"

            "   -o s e r: find reachability from s to e with the expected "
            "result r\n"

            "   --dijkstra: run the dijkstra algorithm to complete the orders\n"

            "   --thorup: run the thorup algorithm to complete the orders\n"

            "   --tabular: run the table algorithm to complete the orders\n"

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
    order_t *order;
    function_t function = { .graph_file = NULL, .orders = linked_list_init(),
            .algorithms = linked_list_init() };

    for (i = 1; i < argc; i++) {
        arg = argv[i];

        if (strcmp("-h", arg) == 0 || strcmp("--help", arg) == 0) {
            usage(argv[0]);
        } else if (strcmp("-v", arg) == 0 || strcmp("--version", arg) == 0) {
            version();
        } else if (strcmp("-o", arg) == 0 || strcmp("--order", arg) == 0) {
            if (argc - i < 3)
                usage(argv[0]);

            MALLOC(order, sizeof(order_t));
            if (sscanf(argv[i+1], "%" PRIu32, &order->start) != 1)
                usage(argv[0]);
            if (sscanf(argv[i+2], "%" PRIu32, &order->end) != 1)
                usage(argv[0]);
            if (sscanf(argv[i+3], "%d", &order->reaches) != 1)
                usage(argv[0]);

            linked_list_add_end(&function.orders, order);

            i += 3; /* Go to next argument. */
        } else if (strcmp("--dijkstra", arg) == 0) {
            linked_list_add_int_end(&function.algorithms, ALGO_DIJKSTRA);
        } else if (strcmp("--tabular", arg) == 0) {
            linked_list_add_int_end(&function.algorithms, ALGO_TABLE);
        } else if (strcmp("--thorup", arg) == 0) {
            linked_list_add_int_end(&function.algorithms, ALGO_THORUP);
        } else {
            function.graph_file = arg;
        }
    }

    return function;
}

static int run_dijkstra(digraph_t *graph, linked_list_t const orders)
{
    path_t path;
    order_t const *order;
    actual_list_t *element;

    printf("running dijkstra\n");

    init_mem_record("dijkstra_mem.txt");

    for (element = orders.start; element != NULL; element = element->next) {
        order = element->element;

        printf("running order %u - %u with result %d\n", order->start,
                order->end, order->reaches);

        dijkstra(&path, graph, order->start, order->end);
    }

    free_mem_record();

    printf("after dijkstra\n");

    return 0;
}

static int run_tabular(digraph_t *graph, linked_list_t const orders)
{
    table_reachability_t table;
    order_t const *order;
    actual_list_t *element;

    init_mem_record("tabular_mem.txt");

    table = table_init(graph);

    for (element = orders.start; element != NULL; element = element->next) {
        order = element->element;

        printf("running order %u - %u with result %d\n", order->start,
                order->end, order->reaches);

        table_reaches(&table, find_vertex(graph, order->start),
                find_vertex(graph, order->end));
    }

    table_free(&table);
    free_mem_record();

    return 0;
}

static int run_thorup(digraph_t *graph, linked_list_t const orders)
{
    actual_list_t *element;
    order_t const *order;

    init_mem_record("thorup_mem.txt");

    for (element = orders.start; element != NULL; element = element->next) {
        order = element->element;

        printf("running order %u - %u with result %d\n", order->start,
                order->end, order->reaches);
    }

    free_mem_record();

    return 0;
}

/*static int monitor(pid_t process, char const *outfilepath)*/
/*{*/
    /*memory_usage_t memstat;*/

    /*memstat_init(&memstat, process);*/

    /*while (waitpid(process, NULL, WNOHANG) == 0) {*/
        /*if (usleep(100000) == -1)*/
            /*error_code(ERR_SLEEP, "Could not sleep");*/

        /*if (memstat_update(&memstat) != 0)*/
            /*continue;*/

        /*printf("pages used %" PRIu32 "\n", memstat.program_size);*/
    /*}*/

    /*memstat_free(&memstat);*/

    /*return EXIT_SUCCESS;*/
/*}*/

static void run_algorithm(digraph_t *graph, uint32_t algorithm,
        linked_list_t const orders)
{
    switch (algorithm) {
    case ALGO_DIJKSTRA:
        run_dijkstra(graph, orders);
        break;
    case ALGO_TABLE:
        run_tabular(graph, orders);
        break;
    case ALGO_THORUP:
        run_thorup(graph, orders);
        break;
    }
}

int main(int argc, char const *argv[])
{
    digraph_t graph;
    function_t function = parse_args(argc, argv);
    actual_list_t *element;
    linked_list_t algorithms = function.algorithms;

    for (element = algorithms.start; element != NULL; element = element->next) {
        vertices_init();
        graph = read_graph(function.graph_file);

        run_algorithm(&graph, element->int_element, function.orders);

        graph_free(&graph);
        vertices_free();
    }

    /* Free resources used in the function. */
    free_functions(&function);

    return EXIT_SUCCESS;
}

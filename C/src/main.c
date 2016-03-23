#include "graph.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    graph_t graph;
    vertex_id_t v0, v1, v2, v3, v4, v5;
    path_t path;

    graph_init(&graph);

    /* Create test graph. */
    v0 = graph_add_vertex(&graph);
    v1 = graph_add_vertex(&graph);
    v2 = graph_add_vertex(&graph);
    v3 = graph_add_vertex(&graph);
    v4 = graph_add_vertex(&graph);
    v5 = graph_add_vertex(&graph);

    graph_add_edge(&graph, v0, v1, 5);
    graph_add_edge(&graph, v0, v3, 1);
    graph_add_edge(&graph, v1, v2, 2);
    graph_add_edge(&graph, v1, v3, 7);
    graph_add_edge(&graph, v2, v1, 4);
    graph_add_edge(&graph, v2, v5, 4);
    graph_add_edge(&graph, v3, v2, 9);
    graph_add_edge(&graph, v3, v4, 12);
    graph_add_edge(&graph, v4, v2, 3);
    graph_add_edge(&graph, v4, v5, 2);

    dijkstra(&path, &graph, v0, v5);

    printf("path length %u\n", path.length);

    path_free(&path);
    graph_free(&graph);

    return EXIT_SUCCESS;
}

/*#include "error.h"*/
/*#include "graph.h"*/
/*#include "CA_graph.h"*/
/*#include <stdio.h>*/
/*#include <stdlib.h>*/
/*#include <string.h>*/
/*#include <unistd.h>*/

/*typedef struct {*/
    /*[> Used to construct a graph from the data given. <]*/
    /*int (*constructor)(graph_t *graph);*/

    /*[> Input data is stored here. <]*/
    /*FILE *file;*/
/*} args_t;*/

/*int argc;*/
/*char const **argv;*/
/*args_t args = { .constructor = NULL, .file = NULL };*/

/*[> Help functions. <]*/
/*static void parse_args();*/

/*int main(int argc, char const *argv[])*/
/*{*/
    /*int retval;*/
    /*graph_t graph;*/

    /*argc = argc;*/
    /*argv = argv;*/
    /*parse_args();*/

    /*if ((retval = args.constructor(&graph)) != 0)*/
        /*error_code(retval);*/

    /*return EXIT_SUCCESS;*/
/*}*/

/*#define HAS_EXTRA_ARG(i) (i < argc-1)*/
/*#define HAS_ARG(i) (i < argc)*/
/*static void handle_types(int *index);*/
/*static void handle_help(int *index);*/
/*static void handle_file(int *index);*/

/*static void parse_args()*/
/*{*/
    /*int i = 0;*/
    /*char const *arg;*/

    /*if (!HAS_ARG(i))*/
        /*usage(argv[0]);*/

    /*for (i = 0; i < argc; i++) {*/
        /*arg = argv[i];*/

        /*if (strcmp(arg, "-t") == 0 || strcmp(arg, "--type") == 0) {*/
            /*handle_types(&i);*/
        /*} else if (strcmp(arg, "-h") == 0 || strcmp(arg, "--help") == 0) {*/
            /*handle_help(&i);*/
        /*} else {*/
            /*handle_file(&i);*/
        /*}*/
    /*}*/
/*}*/

/*static void handle_types(int *index)*/
/*{*/
    /*int i = *index;*/
    /*char const *type;*/

    /*if (!HAS_EXTRA_ARG(i))*/
        /*usage(argv[0]);*/
    /*else*/
        /*type = argv[++i];*/

    /*if (args.constructor != NULL)*/
        /*usage(argv[0]);*/

    /*if (strcmp(type, "CA") == 0) {*/
        /*args.constructor = ca_constructor;*/
        /**index = i;*/
    /*} else {*/
        /*usage(argv[0]);*/
    /*}*/

/*}*/

/*static void handle_help(int *index)*/
/*{*/
    /*usage(argv[0]);*/
/*}*/

/*static void handle_file(int *index)*/
/*{*/
    /*FILE *file;*/
    /*int i = *index;*/

    /*if (args.file != NULL)*/
        /*usage(argv[0]);*/

    /*file = fopen(argv[++i], "r");*/

    /*if (file == NULL) {*/
        /*usage(argv[0]);*/
    /*} else {*/
        /*args.file = file;*/
        /**index = i;*/
    /*}*/

/*}*/
/*#undef HAS_EXTRA_ARG*/

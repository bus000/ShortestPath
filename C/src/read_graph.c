#include "graph.h"
#include "mem_man.h"
#include "file.h"
#include "error.h"
#include "map.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int cmp_vertex(void const *v1, void const *v2)
{
    uint32_t const *vertex1 = v1, *vertex2 = v2;

    if (*vertex1 < *vertex2)
        return -1;
    else if (*vertex1 > *vertex2)
        return 1;
    else
        return 0;
}

static int hash_vertex(void const *v)
{
    uint32_t const *vertex = v;

    return *vertex;
}

digraph_t read_graph(char const *graph_file)
{
    digraph_t graph;
    char *content, *l;
    file_t file;
    uint32_t v0, v1, *newvertex;
    vertex_t *vertex0, *vertex1;
    map_t vertices;

    map_init(&vertices, 1024, hash_vertex, cmp_vertex);
    file_init(&file, graph_file);
    graph_init(&graph);

    content = file_read(&file);

    for (l = strtok(content, "\n"); l != NULL; l = strtok(NULL, "\n")) {
        if (sscanf(l, "%u\t%u", &v0, &v1) != 2)
            error_code(ERR_FORMAT, "graph file not correct format '%s' \n", l);

        vertex0 = (vertex_t *) map_get(&vertices, &v0);
        vertex1 = (vertex_t *) map_get(&vertices, &v1);

        if (vertex0 == NULL) {
            MALLOC(newvertex, sizeof(*newvertex));
            *newvertex = v0;
            vertex0 = new_vertex_id(v0);
            graph_add_vertex_pointer(&graph, vertex0);
            map_put(&vertices, newvertex, vertex0);
        }

        if (vertex1 == NULL) {
            MALLOC(newvertex, sizeof(*newvertex));
            *newvertex = v1;
            vertex1 = new_vertex_id(v1);
            graph_add_vertex_pointer(&graph, vertex1);
            map_put(&vertices, newvertex, vertex1);
        }

        graph_add_edge_pointer(&graph, vertex0, vertex1, 2);
    }

    FREE(content);
    file_free(&file);
    map_free(&vertices);

    return graph;
}

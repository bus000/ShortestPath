#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include "graph.h"

typedef struct {
    uint32_t vertex_number;
} header_t;

static header_t read_header(FILE *graph_file);

digraph_t read_graph(char const *graph_file)
{
    uint32_t i;
    digraph_t graph;
    FILE *file;
    char *line = NULL;
    size_t len = 0, read;
    header_t header;
    vertex_id_t start, end;
    vertex_t **vertices;

    graph_init(&graph);

    file = fopen(graph_file, "r");
    if (file == NULL) {
        fprintf(stderr, "File not found %s\n", graph_file);
        exit(EXIT_FAILURE);
    }

    header = read_header(file);

    vertices = malloc(sizeof(vertex_t *) * header.vertex_number);
    for (i = 0; i < header.vertex_number; i++) {
        vertices[i] = new_vertex();
        graph_add_vertex_pointer(&graph, vertices[i]);
    }

    while ((read = getline(&line, &len, file)) != -1) {
        sscanf(line, "%u\t%u\n", &start, &end);
        graph_add_edge_pointer(&graph, vertices[start], vertices[end], 2);
        graph_add_edge(&graph, start, end, 2);
    }

    fclose(file);

    if (line != NULL)
        free(line);

    return graph;
}

static header_t read_header(FILE *graph_file)
{
    uint32_t i;
    char *line = NULL;
    size_t len;
    uint32_t nodes, edges;
    header_t header;

    /* Ignore 2 lines. */
    for (i = 0; i < 2; i++)
        getline(&line, &len, graph_file);

    getline(&line, &len, graph_file);
    sscanf(line, "# Nodes: %u Edges: %u\n", &nodes, &edges);
    header.vertex_number = nodes;

    /* Ignore 1 line. */
    getline(&line, &len, graph_file);

    return header;
}

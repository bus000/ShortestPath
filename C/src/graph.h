#ifndef GRAPH_H
#define GRAPH_H

#include <inttypes.h>
#include <stdio.h>
#include "path.h"
#include "linked_list.h"
#include "vertex_list.h"

#define INIT_GRAPH_SIZE (128)
#define INIT_LIST_SIZE  (32)

/* Represents a directed graph. */
typedef struct {
    /* A list of vertices in the graph. */
    uint32_t vertices_len;
    uint32_t vertices_size;
    vertex_t **vertices;

    /* Maintain the labels of the vertices. */
    void *labels;
    /* Number of labels. */
    uint32_t labels_size;
    /* Size of each individual label in bytes. */
    size_t label_size;
} digraph_t;

/* FUNCTIONS. */

/* Initialize a new graph. This function has to be called before any using the
 * graph. */
int graph_init(digraph_t *graph);

/* Initialize a new graph with the len vertices given. */
int graph_init_vertices(digraph_t *graph, vertex_t **vertices, uint32_t len);

/* Return true if there is an edge from v1 to v2, false otherwise. */
int graph_adjesent(digraph_t const *graph, vertex_id_t v1, vertex_id_t v2);

/* Add a new vertex to the graph and returns the unique ID of the new vertex. */
vertex_id_t graph_add_vertex(digraph_t *graph);

void graph_add_vertex_pointer(digraph_t *graph, vertex_t *vertex);

/* Adds the edge from the vertices x to y, if it is not there. Returns 0 on
 * success, -1 if one of the edges is not found. */
int graph_add_edge(digraph_t *graph, vertex_id_t v1, vertex_id_t v2,
        uint32_t weight);
void graph_add_edge_pointer(digraph_t *graph, vertex_t *vertex1,
        vertex_t *vertex2, uint32_t weight);

/* Find a vertex with the given identifier in the graph, returns NULL if not
 * found. */
vertex_t * find_vertex(digraph_t const *graph, vertex_id_t v);

/* Free resources used by the graph. */
void graph_free(digraph_t *graph);

/* Returns 0 if a path has been found, -1 if either start or end vertex is not
 * found in the graph. */
int dijkstra(path_t *path, digraph_t *graph, vertex_id_t start, vertex_id_t end);

/* Constructs the list of vertices reachable from the vertex with the ID
 * given. The result is saved in the list pointer which is assumed to be an
 * initialized vertex_list_t. */
void reachable(vertex_list_t *list, vertex_t *vertex);

/* Construct the list of vertices that can reach the vertex given. */
void reaching(vertex_list_t *list, vertex_t *vertex, digraph_t const *graph);

/* Modifies the empty initialized list pointer to contain all the vertices in
 * the graph. The vertices are not copied so should not be changed, but the
 * pointers are and they are allowed to be changed. */
/* TODO: remove? */
linked_list_t graph_vertices(digraph_t const *graph);
vertex_list_t graph_vertices_list(digraph_t const *graph);

/* Remove all vertices in the list of vertices replacing them with a single new
 * vertex, all ingoing edges is replaced by a reference to this new vertex. */
vertex_id_t graph_contract(digraph_t *graph, vertex_list_t *vertices);

/* TODO: find better name and comment on what function does. */
/* TODO: remove? */
vertex_id_t graph_contract2(digraph_t *graph, vertex_list_t *vertices);

vertex_t * graph_first_vertex(digraph_t const *graph);

digraph_t graph_copy(digraph_t const *graph);

void graph_remove_vertices(digraph_t *graph, vertex_list_t *vertices);

digraph_t read_graph(char const *graph_file);

/* Uses depth first search too visit all possible vertices from the first vertex
 * in the graph. If all vertices are reached, true is returned false
 * otherwise. */
int connected_directed(digraph_t *graph);
int connected_undirected(digraph_t *graph);

void graph_remove_edge(digraph_t *graph, vertex_t *start, vertex_t *end);

#endif

#ifndef GRAPH_H
#define GRAPH_H

#include <inttypes.h>
#include <stdio.h>
#include "path.h"
#include "linked_list.h"

#define INIT_GRAPH_SIZE (128)
#define INIT_LIST_SIZE  (32)

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
} graph_t;

typedef struct {
    vertex_t **vertices;
    uint32_t len;
    uint32_t size;
} vertex_list_t;

/* FUNCTIONS. */

/* Initialize a new graph. This function has to be called before any using the
 * graph. */
int graph_init(graph_t *graph);

/* Initialize a new graph with the len vertices given. */
int graph_init_vertices(graph_t *graph, vertex_t **vertices, uint32_t len);

/* Return true if there is an edge from v1 to v2, false otherwise. */
int graph_adjesent(graph_t const *graph, vertex_id_t v1, vertex_id_t v2);

/* Add a new vertex to the graph and returns the unique ID of the new vertex. */
vertex_id_t graph_add_vertex(graph_t *graph);

/* Adds the edge from the vertices x to y, if it is not there. Returns 0 on
 * success, -1 if one of the edges is not found. */
int graph_add_edge(graph_t *graph, vertex_id_t v1, vertex_id_t v2,
        uint32_t weight);

/* Find a vertex with the given identifier in the graph, returns NULL if not
 * found. */
vertex_t * find_vertex(graph_t const *graph, vertex_id_t v);

/* Free resources used by the graph. */
void graph_free(graph_t *graph);

/* Returns 0 if a path has been found, -1 if either start or end vertex is not
 * found in the graph. */
int dijkstra(path_t *path, graph_t *graph, vertex_id_t start, vertex_id_t end);

/* Initializes a new vertex list, this function should be called before the
 * list is used. */
int vertex_list_init(vertex_list_t *list);

/* Constructs the list of vertices reachable from the vertex with the ID
 * given. The result is saved in the list pointer which is assumed to be an
 * initialized vertex_list_t. */
void reachable(vertex_list_t *list, vertex_t *vertex);

/* Construct the list of vertices that can reach the vertex given. */
void reaching(vertex_list_t *list, vertex_t *vertex, graph_t const *graph);

/* Returns true if the list contains a vertex with the given ID, false
 * otherwise. */
int vertex_list_contains(vertex_list_t *list, vertex_id_t v);

/* Add a new vertex to a vertex list. */
int vertex_list_add(vertex_list_t *list, vertex_t *vertex);

/* Free all the resources used by the vertex list. */
void vertex_list_free(vertex_list_t *list);

/* Modifies the empty initialized list pointer to contain all the vertices in
 * the graph. The vertices are not copied so should not be changed, but the
 * pointers are and they are allowed to be changed. */
linked_list_t graph_vertices(graph_t const *graph);

/* Remove all vertices in the list of vertices replacing them with a single new
 * vertex, all ingoing edges is replaced by a reference to this new vertex. */
vertex_id_t graph_contract(graph_t *graph, vertex_list_t *vertices);

/* TODO: find better name and comment on what function does. */
vertex_id_t graph_contract2(graph_t *graph, vertex_list_t *vertices);

#endif

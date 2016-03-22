#ifndef GRAPH_H
#define GRAPH_H

struct Vertex;
struct Edge;
typedef struct Vertex vertex_t;
typedef struct Edge edge_t;

#include <inttypes.h>
#include <stdio.h>
#include "path.h"

#define INIT_GRAPH_SIZE (128)
#define INIT_EDGES_NUM  (8)
#define INIT_LIST_SIZE  (32)

struct Vertex {
    /* A graph unique ID of a vertex. */
    uint32_t unique_id;

    /* List of edges going out from this vertex. */
    uint32_t edges_len;
    uint32_t edges_size;
    edge_t *edges;

    /* Graph labelling, when changed, it should include a new function to free
     * resources used by the label. */
    void *label;
    void (*free_label)(void *);
    void (*print_label)(void *, FILE *);
};

struct Edge {
    /* The higher the weight, the greater the cost to use the edge. */
    uint32_t weight;

    /* Pointers to the two vertices this edge connects. */
    vertex_t *start;
    vertex_t *end;
};

typedef struct {
    /* The unique ID of the next vertex. */
    uint32_t next_vertex;

    /* A list of vertices in the graph. */
    uint32_t vertices_len;
    uint32_t vertices_size;
    vertex_t *vertices;
} graph_t;

typedef struct {
    vertex_t **vertices;
    uint32_t len;
    uint32_t size;
} vertex_list_t;

typedef uint32_t vertex_id_t;

/* FUNCTIONS. */

/* Initialize a new graph. This function has to be called before any using the
 * graph. */
int graph_init(graph_t *graph);

/* Return true if there is an edge from v1 to v2, false otherwise. */
int graph_adjesent(graph_t const *graph, vertex_id_t v1, vertex_id_t v2);

/* Add a new vertex to the graph and returns the unique ID of the new vertex. */
vertex_id_t graph_add_vertex(graph_t *graph);

/* Adds the edge from the vertices x to y, if it is not there. Returns 0 on
 * success, -1 if one of the edges is not found. */
int graph_add_edge(graph_t *graph, vertex_id_t v1, vertex_id_t v2,
        uint32_t weight);

/* Returns the label associated with the vertex x. Returns pointer to label if
 * vertex is found (might be NULL) or NULL if the vertex is not found. */
void * graph_get_label(graph_t const *graph, vertex_id_t v);

/* Set the label of a vertex in a graph. The old label is first freed and the
 * new is then inserted. The function free_label should take the label just
 * inserted and free all resources used by it. The function returns 0 on
 * success, and -1 if the vertex is not found. */
int graph_set_vertex_label(graph_t *graph, vertex_id_t v, void *label,
        void (*free_label)(void *));

/* Set all labels in the graph to the same value. The function free_label should
 * take the label type and free all resources used by it. The function
 * print_label, should take a label and a file and print the label to the
 * file. */
void graph_set_all_labels(graph_t *graph, void *label,
        void (*free_label)(void *),
        void (*print_label)(void *, FILE *));

/* Set all labels in the graph to NULL. */
void graph_null_all_labels(graph_t *graph);

/* Set all labels as the labels generated by the create label function when
 * passing the common pointer given and the ID of the vertex currently
 * generating labels for. */
void graph_set_all_labels_f(graph_t *graph, void *common,
        void * (*create_label)(void *common, vertex_id_t v),
        void (*free_label)(void *),
        void (*print_label)(void *, FILE *));

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

/* Free all the resources used by the vertex list. */
void vertex_list_free(vertex_list_t *list);

/* MISC. */

/* A nop function for adding to graphs where label freeing is not necessary. */
void free_null_label(void *label);

#endif

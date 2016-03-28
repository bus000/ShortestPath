#ifndef VERTEX_H
#define VERTEX_H

#include "inttypes.h"
#include <stdio.h>

#define INIT_VERTICES_SIZE (128)
#define INIT_EDGES_NUM  (8)

struct Vertex;
struct Edge;
typedef struct Vertex vertex_t;
typedef struct Edge edge_t;
typedef uint32_t vertex_id_t;

struct Vertex {
    /* A graph unique ID of a vertex. */
    vertex_id_t unique_id;

    /* List of edges going out from this vertex. */
    uint32_t edges_len;
    uint32_t edges_size;
    edge_t *edges;

    /* Graph labelling, when changed, it should include a new function to free
     * resources used by the label. */
    void *label;
    void (*free_label)(void *);
    void (*print_label)(void *, FILE *);

    /* Special label just for marking if a vertex is visited or not. */
    int visited;
};

struct Edge {
    /* The higher the weight, the greater the cost to use the edge. */
    uint32_t weight;

    /* Pointers to the two vertices this edge connects. */
    vertex_t *start;
    vertex_t *end;
};

/* Should be called before any vertices are used. The function initializes the
 * memory in which all vertices are going to be held. */
void vertices_init(void);

/* Construct a new vertex and return a pointer to it. NULL if the vertex could
 * not be created. */
vertex_t * new_vertex(void);

/* A nop function for adding to graphs where label freeing is not necessary. */
void free_null_label(void *label);

#endif

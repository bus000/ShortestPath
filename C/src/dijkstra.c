#include "graph.h"
#include "heap.h"
#include "error.h"
#include "graph_labeling.h"
#include <stdlib.h>
#include <stdio.h>

/* The label given to the vertices in the graph when using Dijkstra. */
typedef struct DijkstraLabel {
    /* True if the distance to the vertex is infinity, false otherwise. */
    int infinity;

    /* If the distance is not infinity, the distance is the weight. */
    uint32_t weight;

    /* Pointer to the previous vertex in the shortest path. NULL if not
     * known. */
    struct DijkstraLabel *prev;
} dijkstra_l_t;

static dijkstra_l_t default_label = { .infinity = 1, .weight = 0, .prev = NULL };
static dijkstra_l_t start_label = { .infinity = 0, .weight = 0, .prev = NULL };

/* Compare two Dijkstra labels, returns 0 if equal, 1 if l1 > l2 and -1 if
 * l1 < l2. */
static int compare_labels(dijkstra_l_t const *l1, dijkstra_l_t const *l2);

/* Compare two vertices on their current distance. Returns 0 if equal, 1 if v1
 * is farther away than v2 and -1 otherwise. */
static int compare_vertices(void const *v1, void const *v2);

/* Used to decrease the weight of a Dijkstra label contained in a heap, sets
 * infinity to false and the weight as the new weight given. */
static void decrease_weight(void *v, void *w);

/* Returns true if the dist given is less than the distance already in the
 * label. Any distance is considered smaller than infinity. */
static inline int shorter_dist(dijkstra_l_t *label, uint32_t dist);

/* Returns true as long as the Dijkstra algorithm should continue. The algorithm
 * stops when current is NULL, current is end or the current node is infinitely
 * far away. */
static inline int dijkstra_finish(vertex_t *current, vertex_id_t end);

/* Runs the Dijkstra algorithm on the graph given and stores the result in the
 * path pointer. Returns 0 if a path is found and an error code otherwise. The
 * min heap should be a heap of all vertices sorted by their current distances
 * in their Dijkstra labels, the end vertex should be the vertex ID of where the
 * algorithm should search for a path to. */
static int dijkstra_algo(path_t *path, digraph_t *graph, vertex_id_t end_vertex,
        min_heap_t *vertices);

/* TODO: Make prettier. */
int dijkstra(path_t *path, digraph_t *graph, vertex_id_t start, vertex_id_t end)
{
    vertex_t *start_vertex = find_vertex(graph, start);
    vertex_t *end_vertex = find_vertex(graph, end);
    dijkstra_l_t *end_vertex_label;
    min_heap_t heap;
    uint32_t i;
    vertex_t **vertex_pointers;
    int ret_code;

    if (start_vertex == NULL || end_vertex == NULL)
        return -1;

    /* Prepare for running the algorithm. */
    path_init(path);
    graph_init_labels(graph, &default_label, sizeof(dijkstra_l_t));
    graph_set_label(graph, start_vertex, &start_label);
    vertex_pointers = malloc(sizeof(vertex_t *) * graph->vertices_len);

    if (vertex_pointers == NULL)
        mem_err();

    for (i = 0; i < graph->vertices_len; i++) {
        vertex_pointers[i] = graph->vertices[i];
    }

    heap_init(&heap, (void **) vertex_pointers, graph->vertices_len,
            compare_vertices, decrease_weight);

    ret_code = dijkstra_algo(path, graph, end, &heap);
    end_vertex_label = (dijkstra_l_t *) end_vertex->label;
    path->length = end_vertex_label->weight;

    free(vertex_pointers);

    return ret_code;
}

static int dijkstra_algo(path_t *path, digraph_t *graph, vertex_id_t end_vertex,
        min_heap_t *vertices)
{
    int i;
    vertex_t *current = heap_extract_min(vertices), *end;
    dijkstra_l_t *current_label = current->label, *end_label;
    uint32_t current_dist = current_label->weight, newdist;
    edge_t *edges = current->outgoing, *edge;

    while (dijkstra_finish(current, end_vertex)) {
        /* Loop through edges. */
        for (i = 0; i < current->outgoing_len; i++) {
            edge = &edges[i];
            end = edge->end;
            end_label = (dijkstra_l_t *) end->label;
            newdist = current_dist + edge->weight;

            if (shorter_dist(end_label, newdist))
                heap_decrease_element(vertices, end, &newdist);
        }

        /* Update current node to new shortest distance. */
        current = heap_extract_min(vertices);
        current_label = current->label;
        edges = current->outgoing;
        current_dist = current_label->weight;
    }

    if (current->unique_id == end_vertex)
        return 0;
    else
        return -1;
}

static int compare_labels(dijkstra_l_t const *l1, dijkstra_l_t const *l2)
{
    if (l1->infinity && l2->infinity)
        return 0;
    else if (l1->infinity)
        return 1;
    else if (l2->infinity)
        return -1;
    else if (l1->weight > l2->weight)
        return 1;
    else if (l1->weight < l2->weight)
        return -1;
    else
        return 0;
}

static int compare_vertices(void const *v1, void const *v2)
{
    vertex_t const *vertex1 = (vertex_t const *) v1;
    vertex_t const *vertex2 = (vertex_t const *) v2;
    dijkstra_l_t const *label1 = vertex1->label;
    dijkstra_l_t const *label2 = vertex2->label;

    return compare_labels(label1, label2);
}

static void decrease_weight(void *v, void *w)
{
    vertex_t *vertex = (vertex_t *) v;
    dijkstra_l_t *label = vertex->label;
    uint32_t *weight = (uint32_t *) w;

    label->infinity = 0;
    label->weight = *weight;
}

static inline int shorter_dist(dijkstra_l_t *label, uint32_t dist)
{
    return label->infinity ? 1 : label->weight > dist;
}

static inline int dijkstra_finish(vertex_t *current, vertex_id_t end)
{
    dijkstra_l_t *current_label = (dijkstra_l_t *) current->label;

    return current != NULL &&
        current->unique_id != end &&
        !current_label->infinity;
}

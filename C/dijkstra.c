#include "graph.h"
#include "heap.h"
#include "error.h"
#include <stdlib.h>
#include <stdio.h>

typedef struct DijkstraLabel {
    int infinity;
    uint32_t weight;
    struct DijkstraLabel *prev;
} dijkstra_label_t;

typedef struct {
    dijkstra_label_t *labels;
    vertex_id_t start;
} dijkstra_init_data_t;

static void print_dijkstra_label(void *l, FILE *f)
{
    dijkstra_label_t *label = (dijkstra_label_t *) l;

    fprintf(f, "%p = { .infinity = %d, weight = %u, prev = %p }",
            l, label->infinity, label->weight, label->prev);
}

static void * dijkstra_label(void *common, vertex_id_t v)
{
    dijkstra_init_data_t *init_data = (dijkstra_init_data_t *) common;
    vertex_id_t start = init_data->start;
    dijkstra_label_t *label_array = init_data->labels;
    dijkstra_label_t *label = &label_array[v];

    if (v == start) {
        label->infinity = 0;
        label->weight = 0;
        label->prev = NULL;
    } else {
        label->infinity = 1;
        label->weight = 0;
        label->prev = NULL;
    }

    return label;
}

static int compare_labels(dijkstra_label_t const *l1, dijkstra_label_t const *l2)
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
    dijkstra_label_t const *label1 = vertex1->label;
    dijkstra_label_t const *label2 = vertex2->label;

    return compare_labels(label1, label2);
}

static void decrease_weight(void *v, void *w)
{
    vertex_t *vertex = (vertex_t *) v;
    dijkstra_label_t *label = vertex->label;
    uint32_t *weight = (uint32_t *) w;

    label->infinity = 0;
    label->weight = *weight;
}

static inline int shorter_dist(dijkstra_label_t *label, uint32_t dist)
{
    return label->infinity ? 1 : label->weight > dist;
}

static inline int dijkstra_finish(vertex_t *current, vertex_id_t end)
{
    dijkstra_label_t *current_label = (dijkstra_label_t *) current->label;

    return current != NULL &&
        current->unique_id != end &&
        !current_label->infinity;
}

static int dijkstra_algo(path_t *path, graph_t *graph, vertex_id_t end_vertex,
        min_heap_t *vertices)
{
    int i;
    vertex_t *current = heap_extract_min(vertices), *end;
    dijkstra_label_t *current_label = current->label, *end_label;
    uint32_t current_dist = current_label->weight, newdist;
    edge_t *edges = current->edges, *edge;

    while (dijkstra_finish(current, end_vertex)) {
        /* Loop through edges. */
        for (i = 0; i < current->edges_len; i++) {
            edge = &edges[i];
            end = edge->end;
            end_label = (dijkstra_label_t *) end->label;
            newdist = current_dist + edge->weight;

            if (shorter_dist(end_label, newdist))
                heap_decrease_element(vertices, end, &newdist);
        }

        /* Update current node to new shortest distance. */
        current = heap_extract_min(vertices);
        current_label = current->label;
        edges = current->edges;
        current_dist = current_label->weight;
    }

    if (current->unique_id == end_vertex)
        return 0;
    else
        return -1;
}

int dijkstra(path_t *path, graph_t *graph, vertex_id_t start, vertex_id_t end)
{
    vertex_t *start_vertex = find_vertex(graph, start);
    vertex_t *end_vertex = find_vertex(graph, end);
    dijkstra_label_t *end_vertex_label;
    dijkstra_label_t *labels;
    min_heap_t heap;
    int ret_code;
    int i;
    dijkstra_init_data_t init_data;
    vertex_t **vertex_pointers;

    if (start_vertex == NULL || end_vertex == NULL)
        return -1;

    /* Prepare for running the algorithm. */
    path_init(path);
    labels = malloc(sizeof(dijkstra_label_t) * graph->vertices_len);
    vertex_pointers = malloc(sizeof(vertex_t *) * graph->vertices_len);
    init_data.labels = labels;
    init_data.start = start;

    if (labels == NULL || vertex_pointers == NULL)
        mem_err();

    for (i = 0; i < graph->vertices_len; i++) {
        vertex_pointers[i] = &graph->vertices[i];
    }

    graph_set_all_labels_f(graph, &init_data, dijkstra_label, free_null_label,
            print_dijkstra_label);
    heap_init(&heap, (void **) vertex_pointers, graph->vertices_len, compare_vertices,
            decrease_weight);

    ret_code = dijkstra_algo(path, graph, end, &heap);
    end_vertex_label = (dijkstra_label_t *) end_vertex->label;
    path->length = end_vertex_label->weight;

    free(labels);

    return ret_code;
}

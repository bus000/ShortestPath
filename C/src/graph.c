#include "graph.h"
#include "error.h"
#include <stdlib.h>

int graph_init(graph_t *graph)
{
    graph->vertices_len = 0;
    graph->vertices_size = INIT_GRAPH_SIZE;

    if ((graph->vertices = malloc(sizeof(vertex_t) * INIT_GRAPH_SIZE)) == NULL)
        mem_err();

    return 0;
}

int graph_adjesent(graph_t const *graph, vertex_id_t v1, vertex_id_t v2)
{
    int i;
    vertex_t *vertex1 = find_vertex(graph, v1);
    edge_t edge;

    if (vertex1 == NULL) {
        return 0;
    }

    for (i = 0; i < vertex1->edges_len; i++) {
        edge = vertex1->edges[i];

        if (edge.end->unique_id == v2)
            return 1;
    }

    return 0;
}

vertex_id_t graph_add_vertex(graph_t *graph)
{
    vertex_t vertex = { .unique_id = get_unique_id(),
        .edges_len = 0,
        .edges_size = INIT_EDGES_NUM,
        .edges = malloc(sizeof(edge_t) * INIT_EDGES_NUM),
        .label = NULL,
        .free_label = free_null_label,
        .print_label = NULL,
        .visited = 0 };

    if (vertex.edges == NULL)
        mem_err();

    if (graph->vertices_len >= graph->vertices_size) {
        graph->vertices_size *= 2;
        graph->vertices = realloc(graph->vertices, graph->vertices_size);

        if (graph->vertices == NULL)
            mem_err();
    }

    graph->vertices[graph->vertices_len] = vertex;
    graph->vertices_len += 1;

    return vertex.unique_id;
}

int graph_add_edge(graph_t *graph, vertex_id_t v1, vertex_id_t v2,
        uint32_t weight)
{
    vertex_t *vertex1 = find_vertex(graph, v1);
    vertex_t *vertex2 = find_vertex(graph, v2);
    edge_t edge = { .weight = weight, .start = vertex1, .end = vertex2 };

    if (vertex1 == NULL || vertex2 == NULL)
        return -1;

    if (vertex1->edges_len >= vertex1->edges_size) {
        vertex1->edges_size *= 2;
        vertex1->edges = realloc(vertex1->edges, vertex1->edges_size);
    }

    vertex1->edges[vertex1->edges_len] = edge;
    vertex1->edges_len += 1;

    return 0;
}

void * graph_get_label(graph_t const *graph, vertex_id_t v)
{
    vertex_t *vertex = find_vertex(graph, v);

    return vertex == NULL ? NULL : vertex->label;
}

int graph_set_vertex_label(graph_t *graph, vertex_id_t v, void *label,
        void (*free_label)(void *))
{
    vertex_t *vertex = find_vertex(graph, v);

    if (vertex == NULL)
        return -1;

    vertex->free_label(vertex->label);

    vertex->label = label;
    vertex->free_label = free_label;

    return 0;
}

void graph_set_all_labels(graph_t *graph, void *label,
        void (*free_label)(void *),
        void (*print_label)(void *, FILE *))
{
    int i;
    vertex_t *vertex;

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = &graph->vertices[i];
        vertex->free_label(vertex->label);
        vertex->label = label;
        vertex->free_label = free_label;
        vertex->print_label = print_label;
    }
}

void graph_null_all_labels(graph_t *graph)
{
    graph_set_all_labels(graph, NULL, free_null_label, NULL);
}

void graph_set_all_labels_f(graph_t *graph, void * common,
        void * (*create_label)(void *common, vertex_id_t v),
        void (*free_label)(void *),
        void (*print_label)(void *, FILE *))
{
    uint32_t i;
    vertex_t *vertex;
    void *label;

    for (i = 0; i < graph->vertices_len; i++) {
        label = create_label(common, i);
        vertex = &graph->vertices[i];

        vertex->free_label(vertex->label);

        vertex->label = label;
        vertex->free_label = free_label;
        vertex->print_label = print_label;
    }
}

void graph_free(graph_t *graph)
{
    int i;
    vertex_t *vertex;

    /* Free labels, and edges. */
    for (i = 0; i < graph->vertices_len; i++) {
        vertex = &graph->vertices[i];
        vertex->free_label(vertex->label);
        free(vertex->edges);
    }

    /* Free vertices. */
    free(graph->vertices);
}

vertex_t * find_vertex(graph_t const *graph, vertex_id_t v)
{
    int i;
    vertex_t *vertex;

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = &(graph->vertices[i]);
        if (vertex->unique_id == v)
            return vertex;
    }

    return NULL;
}

/* Help function for reachable. */
static void reachable_prime(vertex_list_t *list, vertex_t *vertex)
{
    int i;
    edge_t edge;
    vertex_t **vertices;

    /* Don't run into an infinite loop. */
    if (vertex->visited)
        return;

    /* Resize vertex list if necessary. */
    if (list->len == list->size) {
        vertices = list->vertices;
        list->size *= 2;
        list->vertices = realloc(vertices, sizeof(vertex_t *) * list->size);
        if (list->vertices == NULL)
            mem_err();
    }

    vertex->visited = 1;
    list->vertices[list->len] = vertex;
    list->len += 1;

    for (i = 0; i < vertex->edges_len; i++) {
        edge = vertex->edges[i];
        reachable(list, edge.end);
    }
}

void reachable(vertex_list_t *list, vertex_t *vertex)
{
    int i;
    vertex_t *v;

    reachable_prime(list, vertex);

    /* Cleanup. */
    for (i = 0; i < list->len; i++) {
        v = list->vertices[i];
        v->visited = 0;
    }
}

linked_list_t graph_vertices(graph_t const *graph)
{
    linked_list_t list;

    linked_list_from_array(&list, (void **) graph->vertices,
            graph->vertices_len);

    return list;
}

void free_null_label(void *label)
{
    /* Nop. */
}

vertex_id_t get_unique_id()
{
    static uint32_t unique_id = 0;

    unique_id += 1;

    return unique_id;
}

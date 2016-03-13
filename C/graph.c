#include "graph.h"
#include "error.h"
#include <stdlib.h>

int graph_init(graph_t *graph)
{
    graph->next_vertex = 0;
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
    vertex_t vertex = { .unique_id = graph->next_vertex,
        .edges_len = 0,
        .edges_size = INIT_EDGES_NUM,
        .edges = malloc(sizeof(edge_t) * INIT_EDGES_NUM),
        .label = NULL,
        .free_label = free_null_label,
        .print_label = NULL };

    if (vertex.edges == NULL)
        mem_err();

    if (graph->vertices_len >= graph->vertices_size) {
        graph->vertices_size *= 2;
        graph->vertices = realloc(graph->vertices, graph->vertices_size);

        if (graph->vertices == NULL)
            mem_err();
    }

    graph->vertices[graph->vertices_len] = vertex;
    graph->next_vertex += 1;
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
    return v > graph->vertices_len ? NULL : &(graph->vertices[v]);
}

void free_null_label(void *label)
{
    /* Nop. */
}

void vertex_print(void *v, FILE *f)
{
    vertex_t *vertex = (vertex_t *) v;

    if (vertex->print_label == NULL) {
        fprintf(f, "%p = { .unique_id = %u, .edges_len = %u, .edges_size = %u, "
                ".edges = %p, .label = %p, .free_label = %p }",
                vertex,
                vertex->unique_id,
                vertex->edges_len,
                vertex->edges_size,
                vertex->edges,
                vertex->label,
                vertex->free_label);
    } else {
        fprintf(f, "%p = { .unique_id = %u, .edges_len = %u, .edges_size = %u, "
                ".edges = %p, .label = ",
                vertex,
                vertex->unique_id,
                vertex->edges_len,
                vertex->edges_size,
                vertex->edges);
        vertex->print_label(vertex->label, f);
        fprintf(f, ", .free_label = %p }", vertex->free_label);
    }
}

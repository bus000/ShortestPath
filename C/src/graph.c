#include "graph.h"
#include "error.h"
#include "vertex.h"
#include <stdlib.h>

/* Return true if a path from current to vertex exist, and false otherwise. */
static int does_reach(vertex_t const *current, vertex_t const *vertex);

int graph_init(graph_t *graph)
{
    graph->vertices_len = 0;
    graph->vertices_size = INIT_GRAPH_SIZE;
    graph->vertices = malloc(sizeof(vertex_t *) * INIT_GRAPH_SIZE);

    if (graph->vertices == NULL)
        mem_err();

    graph->labels = NULL;
    graph->labels_size = 0;
    graph->label_size = 0;

    return 0;
}

int graph_init_vertices(graph_t *graph, vertex_t **vertices, uint32_t len)
{
    graph->vertices = vertices;
    graph->vertices_len = len;
    graph->vertices_size = len;

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

    for (i = 0; i < vertex1->outgoing_len; i++) {
        edge = vertex1->outgoing[i];

        if (edge.end->unique_id == v2)
            return 1;
    }

    return 0;
}

void graph_add_vertex_pointer(graph_t *graph, vertex_t *vertex)
{
    size_t newsize;

    if (graph->vertices_len >= graph->vertices_size) {
        graph->vertices_size *= 2;
        newsize = sizeof(vertex_t *) * graph->vertices_size;
        graph->vertices = realloc(graph->vertices, newsize);

        if (graph->vertices == NULL)
            mem_err();
    }

    graph->vertices[graph->vertices_len] = vertex;
    graph->vertices_len += 1;
}

vertex_id_t graph_add_vertex(graph_t *graph)
{
    vertex_t *vertex = new_vertex();

    graph_add_vertex_pointer(graph, vertex);

    return vertex->unique_id;
}

void graph_add_edge_pointer(graph_t *graph, vertex_t *vertex1,
        vertex_t *vertex2, uint32_t weight)
{
    edge_t edge = { .weight = weight, .start = vertex1, .end = vertex2 };

    if (vertex1->outgoing_len >= vertex1->outgoing_size) {
        vertex1->outgoing_size = vertex1->outgoing_size == 0 ? 2 :
            vertex1->outgoing_size * 2;
        vertex1->outgoing = realloc(vertex1->outgoing,
                sizeof(edge_t) *vertex1->outgoing_size);

        if (vertex1->outgoing == NULL)
            mem_err();
    }

    vertex1->outgoing[vertex1->outgoing_len] = edge;
    vertex1->outgoing_len += 1;
}

int graph_add_edge(graph_t *graph, vertex_id_t v1, vertex_id_t v2,
        uint32_t weight)
{
    vertex_t *vertex1 = find_vertex(graph, v1);
    vertex_t *vertex2 = find_vertex(graph, v2);

    if (vertex1 == NULL || vertex2 == NULL)
        return -1;

    graph_add_edge_pointer(graph, vertex1, vertex2, weight);

    return 0;
}

void graph_free(graph_t *graph)
{
    /* Free vertices. */
    free(graph->vertices);

    if (graph->labels_size != 0)
        free(graph->labels);
}

vertex_t * find_vertex(graph_t const *graph, vertex_id_t v)
{
    int i;
    vertex_t *vertex;

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
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

    /* Don't run into an infinite loop. */
    if (vertex->visited)
        return;

    vertex->visited = 1;
    vertex_list_add(list, vertex);

    for (i = 0; i < vertex->outgoing_len; i++) {
        edge = vertex->outgoing[i];
        reachable_prime(list, edge.end);
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

vertex_list_t graph_vertices_list(graph_t const *graph)
{
    vertex_list_t list;

    vertex_list_init_array(&list, graph->vertices, graph->vertices_len);

    return list;
}

vertex_id_t graph_contract(graph_t *graph, vertex_list_t *vertices)
{
    uint32_t i, j, edges_len;
    vertex_t *vertex;
    edge_t *edges, *edge;

    if (vertices->len == 0)
        return 0;

    graph_remove_vertices(graph, vertices);

    /* Create new vertex representing removed vertices. */
    vertex = find_vertex(graph, graph_add_vertex(graph));
    for (i = 0; i < graph->vertices_len; i++) {
        edges = graph->vertices[i]->outgoing;
        edges_len = graph->vertices[i]->outgoing_len;
        for (j = 0; j < edges_len; j++) {
            edge = &(edges[j]);

            if (vertex_list_contains(vertices, edge->end->unique_id))
                edge->end = vertex;
        }
    }

    return vertex->unique_id;
}

/* TODO: Find more descriptive name. */
vertex_id_t graph_contract2(graph_t *graph, vertex_list_t *vertices)
{
    uint32_t i, j, edges_len;
    edge_t *edges, *edge;
    vertex_id_t vertex;

    if (vertices->len == 0)
        return 0;

    graph_remove_vertices(graph, vertices);

    /* Create new vertex representing removed vertices. */
    vertex = graph_add_vertex(graph);
    for (i = 0; i < vertices->len; i++) {
        edges = vertices->vertices[i]->outgoing;
        edges_len = vertices->vertices[i]->outgoing_len;
        for (j = 0; j < edges_len; j++) {
            edge = &(edges[j]);

            if (!vertex_list_contains(vertices, edge->end->unique_id))
                graph_add_edge(graph, vertex, edge->end->unique_id,
                        edge->weight);
        }
    }

    return vertex;
}

/* TODO: When calling does_reach all vertices on the path up until finding the
 * vertex should also be added to the list as they can obviously also reach the
 * vertex. */
void reaching(vertex_list_t *list, vertex_t *vertex, graph_t const *graph)
{
    uint32_t i;
    vertex_t *current;
    vertex_id_t cur_id;

    vertex_list_add(list, vertex);

    for (i = 0; i < graph->vertices_len; i++) {
        current = graph->vertices[i];
        cur_id = current->unique_id;
        if (!vertex_list_contains(list, cur_id) && does_reach(current, vertex))
            vertex_list_add(list, current);
    }
}

static int does_reach(vertex_t const *current, vertex_t const *vertex)
{
    uint32_t i;
    edge_t edge;

    if (current->unique_id == vertex->unique_id)
        return 1;

    for (i = 0; i < current->outgoing_len; i++) {
        edge = current->outgoing[i];

        if (does_reach(edge.end, vertex))
            return 1;
    }

    return 0;
}

void graph_remove_vertices(graph_t *graph, vertex_list_t const *vertices)
{
    uint32_t i;
    uint32_t move = 0;
    vertex_t *vertex;

    if (vertices->len == 0)
        return;

    /* Remove vertices from graph. */
    for (i = 0; i < vertices->len; i++) {
        vertex = vertices->vertices[i];
        vertex->unique_id = 0;
    }

    /* Move all vertices in graph to start of list. */
    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        if (vertex->unique_id == 0) {
            move += 1;
        } else {
            graph->vertices[i-move] = vertex;
        }
    }
    graph->vertices_len -= move;
}

vertex_t * graph_first_vertex(graph_t const *graph)
{
    return graph->vertices_len == 0 ? NULL : graph->vertices[0];
}

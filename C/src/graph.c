#include "graph.h"
#include "error.h"
#include "stack.h"
#include "vertex.h"
#include "mem_man.h"
#include "queue.h"
#include "file.h"
#include <stdlib.h>

static void graph_reset_visited(digraph_t *graph)
{
    vertex_t *vertex;
    uint32_t i;

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        vertex->visited = 0;
    }
}

int reach_DFS(digraph_t *graph, vertex_t const *start, vertex_t const *end)
{
    stack_t stack = stack_init(128); /* Stack of vertex_t *. */
    vertex_t *current, *neighbour;
    uint32_t i;
    int found = 0;

    stack_push(&stack, start);

    while ((current = (vertex_t *) stack_pop(&stack)) != NULL) {
        if (current->visited)
            continue;

        current->visited = 1;

        if (current == end) {
            found = 1;
            break;
        }

        for (i = 0; i < current->outgoing_len; i++) {
            neighbour = current->outgoing[i].end;
            stack_push(&stack, neighbour);
        }
    }

    stack_free(&stack);
    graph_reset_visited(graph);

    return found;
}

int graph_init(digraph_t *graph)
{
    graph->vertices_len = 0;
    graph->vertices_size = INIT_GRAPH_SIZE;
    MALLOC(graph->vertices, sizeof(vertex_t *) * INIT_GRAPH_SIZE);
    graph->edges_len = 0;

    graph->labels = NULL;
    graph->labels_size = 0;
    graph->label_size = 0;

    return 0;
}

int graph_adjesent(digraph_t const *graph, vertex_id_t v1, vertex_id_t v2)
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

void graph_add_vertex_pointer(digraph_t *graph, vertex_t *vertex)
{
    size_t newsize;

    if (graph->vertices_len >= graph->vertices_size) {
        graph->vertices_size *= 2;
        newsize = sizeof(vertex_t *) * graph->vertices_size;
        REALLOC(graph->vertices, newsize);
    }

    graph->vertices[graph->vertices_len] = vertex;
    vertex->graph_index = graph->vertices_len;
    graph->vertices_len += 1;
}

vertex_id_t graph_add_vertex(digraph_t *graph)
{
    vertex_t *vertex = new_vertex();

    graph_add_vertex_pointer(graph, vertex);

    return vertex->unique_id;
}

void graph_add_edge_pointer(digraph_t *graph, vertex_t *vertex1,
        vertex_t *vertex2, uint32_t weight)
{
    edge_t out = { .weight = weight, .start = vertex1, .end = vertex2,
        .direction = OUTGOING };
    edge_t in = { .weight = weight, .start = vertex2, .end = vertex1,
        .direction = INCOMING };

    /* Add outgoing edge. */
    if (vertex1->outgoing_len >= vertex1->outgoing_size) {
        vertex1->outgoing_size = vertex1->outgoing_size == 0 ? 2 :
            vertex1->outgoing_size * 2;

        REALLOC(vertex1->outgoing, sizeof(edge_t) * vertex1->outgoing_size);
    }

    vertex1->outgoing[vertex1->outgoing_len] = out;
    vertex1->outgoing_len += 1;

    /* Add incoming edge. */
    if (vertex2->incoming_len >= vertex2->incoming_size) {
        vertex2->incoming_size *= 2;
        REALLOC(vertex2->incoming, sizeof(edge_t) * vertex2->incoming_size);
    }

    vertex2->incoming[vertex2->incoming_len] = in;
    vertex2->incoming_len += 1;

    graph->edges_len += 1;
}

int graph_add_edge(digraph_t *graph, vertex_id_t v1, vertex_id_t v2,
        uint32_t weight)
{
    vertex_t *vertex1 = find_vertex(graph, v1);
    vertex_t *vertex2 = find_vertex(graph, v2);

    if (vertex1 == NULL || vertex2 == NULL)
        return -1;

    graph_add_edge_pointer(graph, vertex1, vertex2, weight);

    return 0;
}

void graph_free(digraph_t *graph)
{
    uint32_t i;

    for (i = 0; i < graph->vertices_len; i++)
        vertex_free(graph->vertices[i]);

    /* Free vertices. */
    FREE(graph->vertices);

    if (graph->labels_size != 0)
        FREE(graph->labels);
}

vertex_t * find_vertex(digraph_t const *graph, vertex_id_t v)
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

linked_list_t graph_vertices(digraph_t const *graph)
{
    return linked_list_from_array((void **) graph->vertices,
            graph->vertices_len);
}

vertex_list_t graph_vertices_list(digraph_t const *graph)
{
    vertex_list_t list;

    vertex_list_init_array(&list, graph->vertices, graph->vertices_len);

    return list;
}

/* TODO: change graph_index of all vertices. */
vertex_id_t graph_contract(digraph_t *graph, vertex_list_t *vertices)
{
    uint32_t i, j, weight;
    vertex_t *vertex, *vertex_new;
    edge_t *edge;

    if (vertices->len == 0)
        return 0;

    /* Create new vertex representing removed vertices to be removed. */
    vertex_new = new_vertex();
    graph_add_vertex_pointer(graph, vertex_new);
    for (i = 0; i < vertices->len; i++) {
        vertex = vertices->vertices[i];

        for (j = 0; j < vertex->outgoing_len; j++) {
            edge = &(vertex->outgoing[j]);
            weight = edge->weight;

            if (!vertex_list_contains(vertices, edge->end->unique_id))
                graph_add_edge_pointer(graph, vertex_new, edge->end, weight);
        }

        for (j = 0; j < vertex->incoming_len; j++) {
            edge = &(vertex->incoming[j]);
            weight = edge->weight;

            if (!vertex_list_contains(vertices, edge->end->unique_id))
                graph_add_edge_pointer(graph, edge->end, vertex_new, weight);
        }
    }

    graph_remove_vertices(graph, vertices);

    return vertex_new->unique_id;
}

/* TODO: When calling does_reach all vertices on the path up until finding the
 * vertex should also be added to the list as they can obviously also reach the
 * vertex. */
void reaching(vertex_list_t *list, vertex_t *vertex, digraph_t *graph)
{
    uint32_t i;
    vertex_t *current;
    vertex_id_t cur_id;

    vertex_list_add(list, vertex);

    for (i = 0; i < graph->vertices_len; i++) {
        current = graph->vertices[i];
        cur_id = current->unique_id;
        if (!vertex_list_contains(list, cur_id) &&
                reach_DFS(graph, current, vertex))
            vertex_list_add(list, current);
    }
}

/* TODO: Change graph_index of all vertices. */
void graph_remove_vertices(digraph_t *graph, vertex_list_t *vertices)
{
    uint32_t i, j;
    uint32_t move = 0;
    vertex_t *vertex;
    edge_t edge;

    if (vertices->len == 0)
        return;

    /* Remove all edges involving the vertices to remove. */
    for (i = 0; i < vertices->len; i++) {
        vertex = vertices->vertices[i];
        for (j = 0; j < vertex->incoming_len; j++) {
            edge = vertex->incoming[j];
            graph_remove_edge(graph, edge.end, edge.start);
        }
        for (j = 0; j < vertex->outgoing_len; j++) {
            edge = vertex->outgoing[j];
            graph_remove_edge(graph, edge.start, edge.end);
        }
    }

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

vertex_t * graph_first_vertex(digraph_t const *graph)
{
    return graph->vertices_len == 0 ? NULL : graph->vertices[0];
}

static void directed_dfs(vertex_t *vertex)
{
    uint32_t i;
    vertex_t *newvertex;

    vertex->visited = 1;

    for (i = 0; i < vertex->outgoing_len; i++) {
        newvertex = vertex->outgoing[i].end;
        if (!newvertex->visited)
            directed_dfs(newvertex);
    }
}

static void undirected_dfs(vertex_t *vertex)
{
    uint32_t i;
    vertex_t *newvertex;

    vertex->visited = 1;

    for (i = 0; i < vertex->outgoing_len; i++) {
        newvertex = vertex->outgoing[i].end;
        if (!newvertex->visited)
            undirected_dfs(newvertex);
    }

    for (i = 0; i < vertex->incoming_len; i++) {
        newvertex = vertex->incoming[i].end;
        if (!newvertex->visited)
            undirected_dfs(newvertex);
    }
}

int connected_directed(digraph_t *graph)
{
    uint32_t i;
    int res = 1;
    vertex_t *vertex;

    if (graph->vertices_len == 0)
        return 1;

    directed_dfs(graph->vertices[0]);

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];

        if (!vertex->visited)
            res = 0;

        vertex->visited = 0;
    }

    return res;
}

int connected_undirected(digraph_t *graph)
{
    uint32_t i;
    int res = 1;
    vertex_t *vertex;

    if (graph->vertices_len == 0)
        return 1;

    undirected_dfs(graph->vertices[0]);

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];

        if (!vertex->visited)
            res = 0;

        vertex->visited = 0;
    }

    return res;
}

digraph_t graph_subgraph(digraph_t const *graph, uint32_t size)
{
    uint32_t i, added;
    digraph_t newgraph;
    queue_t queue;
    vertex_t *current, *adjasent, *newvertex;

    graph_init(&newgraph);

    current = graph_first_vertex(graph);
    current->visited = 1;
    queue = queue_singular(current);
    if (queue_empty(&queue))
        printf("queue is empty.\n");

    for (added = 0; added < size && !queue_empty(&queue); ) {
        current = (vertex_t *) dequeue(&queue);

        for (i = 0; i < current->outgoing_len; i++) {
            adjasent = current->outgoing[i].end;
            if (!adjasent->visited) {
                added += 1;
                newvertex = new_vertex_id(adjasent->unique_id);
                graph_add_vertex_pointer(&newgraph, newvertex);
                graph_add_edge(&newgraph, current->unique_id,
                        newvertex->unique_id, 1);

                adjasent->visited = 1;
                enqueue(&queue, adjasent);
            } else {
                graph_add_edge(&newgraph, current->unique_id,
                        adjasent->unique_id, 1);
            }
        }
    }

    graph_reset_visited((digraph_t *) graph);
    queue_free(&queue);

    return newgraph;
}

void graph_remove_edge(digraph_t *graph, vertex_t *start, vertex_t *end)
{
    vertex_remove_outgoing(start, end);
    vertex_remove_incoming(end, start);
    graph->edges_len -= 1;
}

void graph_save(digraph_t const *graph, file_t *file)
{
    char line[128];
    size_t line_size = sizeof(line);
    uint32_t i, j;
    vertex_t const *vertex, *end;
    vertex_id_t v0, v1;

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        v0 = vertex->unique_id;

        for (j = 0; j < vertex->outgoing_len; j++) {
            end = vertex->outgoing[j].end;
            v1 = end->unique_id;
            if (snprintf(line, line_size, "%u\t%u\n", v0, v1) >= line_size)
                error_code(ERR_FORMAT, "snprintf truncate %ld chars",
                        (long long) line_size);

            file_write(file, line);
        }
    }
}

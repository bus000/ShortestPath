#include "error.h"
#include "thorup_dist.h"
#include "graph_labeling.h"
#include "mem_man.h"
#include "util.h"
#include "stack.h"
#include "map.h"
#include <stdlib.h>

/*static uint32_t remove_layer_n = -1; [> 0 <]*/

/*static int lesser_remove(vertex_t const *vertex)*/
/*{*/
    /*thorup_label_t const *label = (thorup_label_t const *) vertex->label;*/

    /*return label->layer > remove_layer_n + 1;*/
/*}*/

/*static void remove_inside(vertex_list_t *list, uint32_t layer)*/
/*{*/
    /*remove_layer_n = layer;*/

    /*vertex_list_filter(list, lesser_remove);*/
/*}*/

/*static int greater_remove(vertex_t const *vertex)*/
/*{*/
    /*thorup_label_t const *label = (thorup_label_t const *) vertex->label;*/

    /*return label->layer < remove_layer_n;*/
/*}*/

/*static void remove_outside(vertex_list_t *list, uint32_t layer)*/
/*{*/
    /*remove_layer_n = layer;*/

    /*vertex_list_filter(list, greater_remove);*/
/*}*/

/*[> Returns a linked list of digraph_t *. <]*/
/*static linked_list_t partition(digraph_t const *graph, uint32_t layers)*/
/*{*/
    /*uint32_t i;*/
    /*vertex_list_t inside;*/
    /*vertex_list_t outside;*/
    /*linked_list_t graphs = linked_list_init();*/
    /*digraph_t *newgraph;*/

    /*for (i = 0; i < layers; i++) {*/
        /*MALLOC(newgraph, sizeof(digraph_t));*/

        /*outside = graph_vertices_list(newgraph);*/
        /*inside = graph_vertices_list(newgraph);*/
        /*remove_inside(&outside, i);*/
        /*remove_outside(&inside, i);*/

        /*graph_remove_vertices(newgraph, &outside);*/
        /*graph_contract(newgraph, &inside);*/

        /*vertex_list_free(&outside);*/
        /*vertex_list_free(&inside);*/

        /*linked_list_add_end(&graphs, newgraph);*/
    /*}*/

    /*[> Free space used on heap. <]*/
    /*vertex_list_free(&inside);*/
    /*vertex_list_free(&outside);*/

    /*return graphs;*/
/*}*/

static vertex_list_t layering_reach(vertex_list_t *vertices, uint32_t layer)
{
    uint32_t i, j;
    stack_t stack = stack_init(128); /* Stack of vertex_t *. */
    vertex_t *current, *neighbour, *start;
    thorup_label_t *label;
    vertex_list_t found;

    vertex_list_init(&found);

    for (i = 0; i < vertices->len; i++) {
        start = vertices->vertices[i];

        for (j = 0; j < start->outgoing_len; j++)
            stack_push(&stack, start->outgoing[j].end);

        while ((current = (vertex_t *) stack_pop(&stack)) != NULL) {
            if (current->visited)
                continue;

            current->visited = 1;
            label = current->label;
            label->layer = layer;
            vertex_list_add(&found, current);

            for (i = 0; i < current->outgoing_len; i++) {
                neighbour = current->outgoing[i].end;
                stack_push(&stack, neighbour);
            }
        }
    }

    stack_free(&stack);

    return found;
}

static vertex_list_t layering_reaching(vertex_list_t *vertices, uint32_t layer)
{
    uint32_t i, j;
    stack_t stack = stack_init(128); /* Stack of vertex_t *. */
    vertex_t *current, *neighbour, *start;
    thorup_label_t *label;
    vertex_list_t found;

    vertex_list_init(&found);

    for (i = 0; i < vertices->len; i++) {
        start = vertices->vertices[i];

        for (j = 0; j < start->incoming_len; j++)
            stack_push(&stack, start->incoming[j].end);

        while ((current = (vertex_t *) stack_pop(&stack)) != NULL) {
            if (current->visited)
                continue;

            current->visited = 1;
            label = current->label;
            label->layer = layer;
            vertex_list_add(&found, current);

            for (i = 0; i < current->incoming_len; i++) {
                neighbour = current->incoming[i].end;
                stack_push(&stack, neighbour);
            }
        }
    }

    stack_free(&stack);

    return found;
}

static thorup_label_t default_label = { .layer = -1 };

uint32_t layering(digraph_t *graph)
{
    uint32_t layer = 0;
    vertex_list_t vertices, tmp;
    vertex_t *start = graph_first_vertex(graph);
    thorup_label_t *label;

    if (start == NULL)
        return 0;

    graph_init_labels(graph, &default_label, sizeof(thorup_label_t));

    vertex_list_init(&vertices);
    start->visited = 1;
    label = start->label;
    label->layer = 0;
    vertex_list_add(&vertices, start);

    while (vertices.len != 0) {
        if (even(layer)) {
            tmp = layering_reach(&vertices, layer);
            vertex_list_free(&vertices);
            vertices = tmp;
        } else {
            tmp = layering_reaching(&vertices, layer);
            vertex_list_free(&vertices);
            vertices = tmp;
        }

        layer += 1;
    }

    return layer-1;
}

static void layer_graph_add(digraph_t *graph, vertex_t *root, vertex_t *add,
        uint32_t inner_layer, map_t *vertices)
{
    uint32_t i;
    vertex_t *neighbour, *existing, *tmp;
    thorup_label_t *label;

    if ((existing = (vertex_t *) map_get(vertices, add)) == NULL) {
        existing = new_vertex_id(add->unique_id);
        graph_add_vertex_pointer(graph, existing);
        map_put(vertices, add, existing);
    }

    for (i = 0; i < add->outgoing_len; i++) {
        neighbour = add->outgoing[i].end;
        label = neighbour->label;

        if (label->layer < inner_layer) { /* Edge should go to root. */
            graph_add_edge_pointer(graph, existing, root, 2);
        } else if (label->layer == inner_layer + 1) { /* Should go in graph. */
            if ((tmp = (vertex_t *) map_get(vertices, neighbour)) == NULL) {
                tmp = new_vertex_id(neighbour->unique_id);
                map_put(vertices, neighbour, tmp);
            }

            neighbour = tmp;
            graph_add_edge_pointer(graph, existing, neighbour, 2);
        }
    }

    for (i = 0; i < add->incoming_len; i++) {
        neighbour = add->incoming[i].end;
        label = neighbour->label;

        if (label->layer < inner_layer) { /* Edge should go to root. */
            graph_add_edge_pointer(graph, root, existing, 2);
        } else if (label->layer == inner_layer + 1) { /* Should go in graph. */
            if ((tmp = (vertex_t *) map_get(vertices, neighbour)) == NULL) {
                tmp = new_vertex_id(neighbour->unique_id);
                map_put(vertices, neighbour, tmp);
            }

            neighbour = tmp;
            graph_add_edge_pointer(graph, neighbour, existing, 2);
        }
    }
}

static int hash(void const *v)
{
    vertex_t const *vertex = (vertex_t const *) v;

    return (int) vertex->unique_id;
}

static int cmp_keys(void const *v1, void const *v2)
{
    vertex_t const *vertex1 = (vertex_t const *) v1;
    vertex_t const *vertex2 = (vertex_t const *) v2;

    if (vertex1->unique_id > vertex2->unique_id)
        return 1;
    else if (vertex1->unique_id < vertex2->unique_id)
        return -1;
    else
        return 0;
}

static digraph_t layer_graph(linked_list_t *inner, linked_list_t *outer)
{
    digraph_t graph;
    vertex_t *root = new_vertex();
    actual_list_t *next;
    vertex_t *vertex;
    uint32_t inner_layer;
    thorup_label_t *label;
    map_t vertices; /* Map of vertex_t pointers. */

    graph_init(&graph);
    graph_add_vertex_pointer(&graph, root);

    next = inner->start;
    if (next != NULL) {
        vertex = (vertex_t *) next->element;
        label = vertex->label;
        inner_layer = label->layer;
    } else {
        return graph;
    }

    map_init(&vertices, 128, hash, cmp_keys);

    for (next = inner->start; next != NULL; next = next->next) {
        vertex = (vertex_t *) next->element;
        layer_graph_add(&graph, root, vertex, inner_layer, &vertices);
    }

    for (next = outer->start; next != NULL; next = next->next) {
        vertex = (vertex_t *) next->element;
        layer_graph_add(&graph, root, vertex, inner_layer, &vertices);
    }

    map_free(&vertices);

    return graph;
}

static linked_list_t split_layers(digraph_t *graph, uint32_t maxlayer)
{
    uint32_t i;
    linked_list_t *layers; /* Array of lists containing vertex_t *. */
    linked_list_t *inner, *outer;
    vertex_t *vertex;
    thorup_label_t *label;
    linked_list_t graphs = linked_list_init();
    digraph_t *newgraph;

    /* Partition the vertices into layers. */
    MALLOC(layers, sizeof(linked_list_t) * (maxlayer + 1));
    for (i = 0; i < maxlayer + 1; i++)
        layers[i] = linked_list_init();

    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        label = vertex->label;

        linked_list_add_end(&layers[label->layer], vertex);
    }

    /* Construct graphs. */
    for (i = 0; i < maxlayer; i++) {
        MALLOC(newgraph, sizeof(digraph_t));
        inner = &layers[i];
        outer = &layers[i+1];
        *newgraph = layer_graph(inner, outer);

        linked_list_add_end(&graphs, newgraph);
    }

    return graphs;
}

int thorup_reach_oracle(reachability_oracle_t *oracle, digraph_t *graph)
{
    linked_list_t graphs;
    uint32_t max_layer = layering(graph);

    graphs = split_layers(graph, max_layer);

    printf("number of graphs %"PRIu64"\n", graphs.len);

    return 0;
}

void reach_oracle_free(reachability_oracle_t *oracle)
{
    linked_list_free(&oracle->graphs);
    linked_list_free(&oracle->spanning_trees);
}

int reachability(reachability_oracle_t const *oracle, vertex_t const *v1,
        vertex_t const *v2)
{
    thorup_label_t const *l1 = (thorup_label_t const *) v1->label;
    thorup_label_t const *l2 = (thorup_label_t const *) v2->label;
    int64_t layer1 = l1->layer;
    int64_t layer2 = l2->layer;

    if (labs(layer1 - layer2) > 1)
        return 0;
    else
        return 1;
}

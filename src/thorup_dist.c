#include "error.h"
#include "thorup_dist.h"
#include "graph_labeling.h"
#include "mem_man.h"
#include "util.h"
#include "stack.h"
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

int layering(digraph_t *graph)
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

    return 0;
}

int thorup_reach_oracle(reachability_oracle_t *oracle, digraph_t *graph)
{
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

#include "error.h"
#include "thorup_dist.h"
#include "graph_labeling.h"
#include "mem_man.h"
#include "util.h"
#include <stdlib.h>

typedef struct {
    uint32_t layer;
} thorup_label_t;

static uint32_t remove_layer_n = -1; /* 0 */

static int lesser_remove(vertex_t const *vertex)
{
    thorup_label_t const *label = (thorup_label_t const *) vertex->label;

    return label->layer > remove_layer_n + 1;
}

static void remove_inside(vertex_list_t *list, uint32_t layer)
{
    remove_layer_n = layer;

    vertex_list_filter(list, lesser_remove);
}

static int greater_remove(vertex_t const *vertex)
{
    thorup_label_t const *label = (thorup_label_t const *) vertex->label;

    return label->layer < remove_layer_n;
}

static void remove_outside(vertex_list_t *list, uint32_t layer)
{
    remove_layer_n = layer;

    vertex_list_filter(list, greater_remove);
}

static void reachable_for_each(vertex_list_t *dest, vertex_list_t *src)
{
    uint32_t i;
    vertex_t *vertex;

    for (i = 0; i < src->len; i++) {
        vertex = src->vertices[i];
        reachable(dest, vertex);
    }
}

static void reaching_for_each(vertex_list_t *dest, vertex_list_t *src,
        digraph_t *graph)
{
    uint32_t i;
    vertex_t *vertex;

    for (i = 0; i < src->len; i++) {
        vertex = src->vertices[i];
        reaching(dest, vertex, graph);
    }
}

/* Returns a linked list of digraph_t *. */
static linked_list_t partition(digraph_t const *graph, uint32_t layers)
{
    uint32_t i;
    vertex_list_t inside;
    vertex_list_t outside;
    linked_list_t graphs = linked_list_init();
    digraph_t *newgraph;

    for (i = 0; i < layers; i++) {
        MALLOC(newgraph, sizeof(digraph_t));

        outside = graph_vertices_list(newgraph);
        inside = graph_vertices_list(newgraph);
        remove_inside(&outside, i);
        remove_outside(&inside, i);

        graph_remove_vertices(newgraph, &outside);
        graph_contract(newgraph, &inside);

        vertex_list_free(&outside);
        vertex_list_free(&inside);

        linked_list_add_end(&graphs, newgraph);
    }

    /* Free space used on heap. */
    vertex_list_free(&inside);
    vertex_list_free(&outside);

    return graphs;
}

static uint32_t set_layer(vertex_list_t *list, uint32_t layer)
{
    uint32_t i;
    vertex_t *vertex;
    thorup_label_t *thorup_label;
    uint32_t changed = 0;

    for (i = 0; i < list->len; i++) {
        vertex = list->vertices[i];
        thorup_label = (thorup_label_t *) vertex->label;
        if (thorup_label->layer == -1) {
            thorup_label->layer = layer;
            changed += 1;
        }
    }

    return changed;
}

static int layering(digraph_t *graph, vertex_t *start)
{
    uint32_t layer;
    vertex_list_t inside, outside, tmp;
    uint32_t changed = 1;

    vertex_list_init(&inside);
    vertex_list_add(&inside, start);
    vertex_list_init(&outside);

    for (layer = 0; changed != 0; layer++) {
        if (even(layer)) {
            reachable_for_each(&outside, &inside);
        } else {
            reaching_for_each(&outside, &inside, graph);
        }

        changed = set_layer(&outside, layer);
        tmp = inside;
        inside = outside;
        outside = tmp;
        vertex_list_empty(&outside);
    }

    /* Free heap space used. */
    vertex_list_free(&inside);
    vertex_list_free(&outside);

    return layer - 1;
}

static thorup_label_t default_label = { .layer = -1 };

int thorup_reach_oracle(reachability_oracle_t *oracle, digraph_t *graph)
{
    vertex_t *start = graph_first_vertex(graph);
    uint32_t layers;
    actual_list_t *next;

    if (start == NULL)
        return -1;

    graph_init_labels(graph, &default_label, sizeof(thorup_label_t));

    layers = layering(graph, start) - 1;
    oracle->graphs = partition(graph, layers);

    oracle->spanning_trees = linked_list_init();
    for (next = oracle->graphs.start; next != NULL; next = next->next) {
        spanning_tree_t *tree = malloc(sizeof(*tree));
        *tree = graph_bf_spanning_tree((digraph_t *) next->element);
        linked_list_add_end(&oracle->spanning_trees, tree);
    }

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

#include "error.h"
#include "thorup_dist.h"
#include "graph_labeling.h"
#include <stdlib.h>

/* Takes a graph and partitions it in to layers. Returns the number of layers
 * partitioned into. */
static int layering(graph_t *graph, vertex_t *start);

/* Set the layer of all vertices in the list to the layer given. */
static int set_layer(vertex_list_t *list, uint32_t layer);

static int even(uint32_t i);

static void reachable_for_each(vertex_list_t *dest, vertex_list_t *src);
static void reaching_for_each(vertex_list_t *dest, vertex_list_t *src,
        graph_t *graph);

static int partition(graph_t const *graph, graph_t *graphs,  uint32_t layers);

typedef struct {
    uint32_t layer;
} thorup_label_t;

static thorup_label_t default_label = { .layer = -1 };

int thorup_reach_oracle(reachability_oracle_t *oracle, graph_t *graph)
{
    vertex_t *start = graph_first_vertex(graph);
    graph_t *graphs;
    uint32_t layers;

    if (start == NULL)
        return -1;

    graph_init_labels(graph, &default_label, sizeof(thorup_label_t));

    layers = layering(graph, start);
    if ((graphs = malloc(sizeof(graph) * layers)) == NULL)
        mem_err();

    partition(graph, graphs, layers);

    return 0;
}

static int partition(graph_t const *graph, graph_t *graphs, uint32_t layers)
{
    uint32_t i, j;
    /*graph_t *current_graph;*/
    vertex_t *vertex;
    thorup_label_t *label;

    for (i = 0; i < layers; i++) {
        /*current_graph = &(graphs[i]);*/

        for (j = 0; j < graph->vertices_len; j++) {
            vertex = graph->vertices[j];
            label = vertex->label;

            if (label->layer == i || label->layer == i + 1) {
                printf("adding vertex %u to graph %u\n", vertex->unique_id-1, i);
            } else if (label->layer < i) {
                printf("adding vertex %u to root %u\n", vertex->unique_id-1, i);
            }
        }
    }

    return 0;
}

static int layering(graph_t *graph, vertex_t *start)
{
    uint32_t layer;
    vertex_list_t inside, outside, tmp;
    int changed = 1;

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

    return layer;
}

static int set_layer(vertex_list_t *list, uint32_t layer)
{
    uint32_t i;
    vertex_t *vertex;
    thorup_label_t *thorup_label;
    int changed = 0;

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

static inline int even(uint32_t i)
{
    return !(i % 2);
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
        graph_t *graph)
{
    uint32_t i;
    vertex_t *vertex;

    for (i = 0; i < src->len; i++) {
        vertex = src->vertices[i];
        reaching(dest, vertex, graph);
    }
}

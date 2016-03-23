#include "error.h"
#include "thorup_dist.h"
#include <stdlib.h>

/* Returns true if input is even and false otherwise. */
static int even(int n);

/* Add all vertices that should be in the layer from the list of vertices
 * vertices to the list layer and removes the same vertices from the list
 * vertices. */
static int layer_even(vertex_list_t *layer, linked_list_t *vertices);

/* Add all vertices that should be in the layer from the list of vertices
 * vertices to the list layer and removes the same vertices from the list
 * vertices. */
static int layer_odd(vertex_list_t *layer, linked_list_t *vertices);

/* Assumes the graph is connected. */
int thorup_layer(graph_t const *graph)
{
    int i;
    vertex_t *start = find_vertex(graph, 0);
    vertex_list_t *layer = malloc(sizeof(vertex_list_t));
    linked_list_t layers, vertices = graph_vertices(graph);

    if (layer == NULL || start == NULL)
        mem_err();

    /* Initialization. */
    linked_list_init(&layers);

    /* Handle label 0. */
    vertex_list_init(layer);
    reachable(layer, start);
    linked_list_add_end(&layers, layer);

    for (i = 0; i < layer->len; i++)
        linked_list_remove(&vertices, layer->vertices[i]);

    /* Handle all other labels. */
    for (i = 1; layers.len != 0; i++) {
        layer = malloc(sizeof(vertex_list_t));
        if (layer == NULL)
            mem_err();

        if (even(i))
            layer_even(layer, &layers);
        else
            layer_odd(layer, &layers);

        linked_list_add_end(&layers, layer);
    }

    /* Free resources used, TODO: free layer and layers and vertices. */

    return 0;
}

static int layer_even(vertex_list_t *layer, linked_list_t *vertices)
{

    return 0;
}

static int layer_odd(vertex_list_t *layer, linked_list_t *vertices)
{

     return 0;
}

static inline int even(int n)
{
    return n % 2 == 0;
}

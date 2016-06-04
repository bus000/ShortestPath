#include "mem_man.h"
#include "error.h"
#include "graph_labeling.h"
#include <stdlib.h>
#include <string.h>

/* TODO: use graph_init_labels_size for the parts that are the same. */
void graph_init_labels(digraph_t *graph, void const *label, size_t label_size)
{
    uint32_t i;
    vertex_t *vertex;
    void *cur_label;

    if (graph->labels_size != 0)
        FREE(graph->labels);

    if (graph->vertices_len == 0)
        return;

    MALLOC(graph->labels, label_size * graph->vertices_len);
    graph->labels_size = graph->vertices_len;
    graph->label_size = label_size;

    /* Initialize all labels with the label pointed to by label. */
    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        cur_label = graph->labels + i * graph->label_size;
        vertex->label = cur_label;
        memcpy(cur_label, label, label_size);
    }
}

void graph_init_labels_size(digraph_t *graph, size_t labels_size,
        size_t label_size)
{
    if (graph->labels_size != 0)
        FREE(graph->labels);

    MALLOC(graph->labels, labels_size);
    graph->labels_size = labels_size / label_size;
    graph->label_size = label_size;
}

int graph_set_label(digraph_t *graph, vertex_t *vertex, void const *label)
{
    if (vertex->label == NULL) {
        return -1;
    } else {
        memcpy(vertex->label, label, graph->label_size);
        return 0;
    }
}
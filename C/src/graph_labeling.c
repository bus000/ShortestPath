#include <stdlib.h>
#include <string.h>
#include "error.h"
#include "graph_labeling.h"

void graph_init_labels(graph_t *graph, void const *label, size_t label_size)
{
    uint32_t i;
    vertex_t *vertex;
    void *cur_label;

    if (graph->labels_size != 0) {
        free(graph->labels);
    }

    graph->labels = malloc(label_size * graph->vertices_len);
    graph->labels_size = graph->vertices_len;
    graph->label_size = label_size;

    if (graph->labels == NULL)
        mem_err();

    /* Initialize all labels with the label pointed to by label. */
    for (i = 0; i < graph->vertices_len; i++) {
        vertex = graph->vertices[i];
        cur_label = graph->labels + i * graph->label_size;
        vertex->label = cur_label;
        memcpy(cur_label, label, label_size);
    }
}

int graph_set_label(graph_t *graph, vertex_t *vertex, void const *label)
{
    if (vertex->label == NULL) {
        return -1;
    } else {
        memcpy(vertex->label, label, graph->label_size);
        return 0;
    }
}

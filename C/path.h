#ifndef PATH_H
#define PATH_H

#include "graph.h"

#define INIT_PATH_LEN (128)

/* Functions finding paths through graphs. */
typedef struct {
    /* The total weight of the edges between all vertices in path. */
    uint32_t length;

    /* List of vertices making up path. */
    uint32_t path_len;
    uint32_t path_size;
    vertex_t **path;
} path_t;

/* Initialize a new path, returns 0 on success. */
int path_init(path_t *path);

/* Free the resources used by a path. */
void path_free(path_t *path);

#endif

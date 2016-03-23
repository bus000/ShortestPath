#include "graph.h"
#include "error.h"
#include <stdlib.h>

void path_free(path_t *path)
{
    if (path->path_size > 0)
        free(path->path);
}

int path_init(path_t *path)
{
    path->length = 0;
    path->path_len = 0;
    path->path_size = INIT_PATH_LEN;
    path->path = malloc(sizeof(vertex_t *) * INIT_PATH_LEN);

    if (path->path == NULL)
        mem_err();

    return 0;
}

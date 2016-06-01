#ifndef MEM_MAN_H
#define MEM_MAN_H

#include <stdlib.h>

#define MALLOC(var, size) do {                                                 \
    var = calloc(1, size);                                                     \
    if (var == NULL)                                                           \
        error_code(ERR_NO_MEM, "Out of memory, %s, %d\n", __FILE__, __LINE__); \
} while (0);

#define REALLOC(var, newsize) do {                                             \
    void *newvar = realloc(var, newsize);                                      \
    if (var == NULL)                                                           \
        error_code(ERR_NO_MEM, "Out of memory, %s, %d\n", __FILE__, __LINE__); \
    var = newvar;                                                              \
} while (0);

#define CALLOC(var, nmemb, size) do {                                          \
    var = calloc(nmemb, size);                                                 \
    if (var == NULL)                                                           \
        error_code(ERR_NO_MEM, "Out of memory, %s, %d\n", __FILE__, __LINE__); \
} while (0);

#define FREE(var) do {                                                         \
    free(var);                                                                 \
} while (0);

#endif

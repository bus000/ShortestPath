#ifndef MEM_MAN_H
#define MEM_MAN_H

#include <stdlib.h>

#ifdef RECORD_MEM
#define MALLOC(var, size) do {                     \
    var = malloc(size);                            \
    if (var == NULL)                               \
        error_code(ERR_NO_MEM, "Out of memory\n"); \
    record_malloc(var, size);                      \
} while (0);
#else
#define MALLOC(var, size) do {                     \
    var = malloc(size);                            \
    if (var == NULL)                               \
        error_code(ERR_NO_MEM, "Out of memory\n"); \
} while (0);
#endif

#ifdef RECORD_MEM
#define REALLOC(var, newsize) do {                 \
    void *newvar = realloc(var, newsize);          \
    if (var == NULL)                               \
        error_code(ERR_NO_MEM, "Out of memory\n"); \
    record_realloc(var, newvar, newsize);          \
    var = newvar;                                  \
} while (0);
#else
#define REALLOC(var, newsize) do {                 \
    void *newvar = realloc(var, newsize);          \
    if (var == NULL)                               \
        error_code(ERR_NO_MEM, "Out of memory\n"); \
    var = newvar;                                  \
} while (0);
#endif

#ifdef RECORD_MEM
#define CALLOC(var, nmemb, size) do {              \
    var = calloc(nmemb, size);                     \
    if (var == NULL)                               \
        error_code(ERR_NO_MEM, "Out of memory\n"); \
    record_calloc(var, nmemb, size);               \
} while (0);
#else
#define CALLOC(var, nmemb, size) do {              \
    var = calloc(nmemb, size);                     \
    if (var == NULL)                               \
        error_code(ERR_NO_MEM, "Out of memory\n"); \
} while (0);
#endif

#ifdef RECORD_MEM
#define FREE(var) do {                             \
    free(var);                                     \
    record_free(var);                              \
} while (0);
#else
#define FREE(var) do {                             \
    free(var);                                     \
} while (0);
#endif

/* TODO: record memory in another thread with a lock so that it does not slow
 * processes down. */

/* Initialize a memory recorder, before that is done, the memory is not
 * recorded. */
void init_mem_record(char const *outfile);

/* Record how many bytes is malloced to a pointer. */
void record_malloc(void const *var, size_t size);

/* Record the size of a resize of a pointer. */
void record_realloc(void const *oldvar, void const *newvar, size_t newsize);

/* Record the size of a calloc allocation. */
void record_calloc(void const *var, size_t nmemb, size_t size);

/* Record a free operation. */
void record_free(void const *var);

/* Stop recording memory usage. */
void free_mem_record(void);

#endif

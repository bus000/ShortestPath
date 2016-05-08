#ifndef ERROR_H
#define ERROR_H

typedef enum {
    ERR_FILE_OPEN = 1,
    ERR_NO_MEM = 2
} error_t;

#define MALLOC(var, size) do {                     \
    var = malloc(size);                            \
    if (var == NULL)                               \
        error_code(ERR_NO_MEM, "Out of memory\n"); \
} while (0);

#define REALLOC(var, newsize) do {                 \
    var = realloc(var, newsize);                   \
    if (var == NULL)                               \
        error_code(ERR_NO_MEM, "Out of memory\n"); \
} while (0);

/* Print error from error code. */
void error_code(error_t error, char const *fmt_msg, ...);

#endif

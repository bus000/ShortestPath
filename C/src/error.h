#ifndef ERROR_H
#define ERROR_H

typedef enum error_e {
    ERR_FILE_OPEN = 1,
    ERR_NO_MEM = 2,
    ERR_SLEEP = 3,
    ERR_FORMAT = 4,
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

#define CALLOC(var, nmemb, size) do {              \
    var = calloc(nmemb, size);                     \
    if (var == NULL)                               \
        error_code(ERR_NO_MEM, "Out of memory\n"); \
} while (0);

/* Print error from error code and exit program. */
void error_code(error_t error, char const *fmt_msg, ...);

/* Print error and return. */
void error_report(error_t error, char const *fmt_msg, ...);

/* Convert an error code to a string. */
char * error_string(error_t error);

#endif

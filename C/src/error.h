#ifndef ERROR_H
#define ERROR_H

typedef enum {
    ERR_FILE_OPEN = 1,
} error_t;

/* Print memory error and exit. */
void mem_err(void);

/* Print error from error code. */
void error_code(error_t error, char const *fmt_msg, ...);

#endif

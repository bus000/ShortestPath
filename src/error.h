#ifndef ERROR_H
#define ERROR_H

typedef enum error_e {
    ERR_FILE_OPEN  = 1,
    ERR_NO_MEM     = 2,
    ERR_SLEEP      = 3,
    ERR_FORMAT     = 4,
    ERR_CONFLICT   = 5,
    ERR_NOT_FOUND  = 6,
    ERR_CONCURRENT = 7,
} error_t;

/* Print error from error code and exit program. */
void error_code(error_t error, char const *fmt_msg, ...);

/* Print error and return. */
void error_report(error_t error, char const *fmt_msg, ...);

/* Convert an error code to a string. */
char * error_string(error_t error);

#endif

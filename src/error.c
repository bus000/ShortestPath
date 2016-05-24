#include "util.h"
#include "error.h"
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdarg.h>

void error_code(error_t error, char const *fmt_msg, ...)
{
    va_list ap;

    va_start(ap, fmt_msg);

    fprintf(stderr, "Error %d - %s\n", error, error_string(error));
    vfprintf(stderr, fmt_msg, ap);

    va_end(ap);

    exit(EXIT_FAILURE);
}

void error_report(error_t error, char const *fmt_msg, ...)
{
    va_list ap;

    va_start(ap, fmt_msg);

    fprintf(stderr, "Error %d - %s\n", error, error_string(error));
    vfprintf(stderr, fmt_msg, ap);

    va_end(ap);
}

char * error_string(error_t error)
{
    switch (error) {
    case ERR_FILE_OPEN:
        return "ERR_FILE_OPEN";
    case ERR_NO_MEM:
        return "ERR_NO_MEM";
    case ERR_SLEEP:
        return "ERR_SLEEP";
    case ERR_FORMAT:
        return "ERR_FORMAT";
    case ERR_CONFLICT:
        return "ERR_CONFLICT";
    case ERR_NOT_FOUND:
        return "ERR_NOT_FOUND";
    case ERR_CONCURRENT:
        return "ERR_CONCURRENT";
    default:
        return "UNKNOWN";
    }
}

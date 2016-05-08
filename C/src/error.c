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

    fprintf(stderr, "Error %d\n", error);
    vfprintf(stderr, fmt_msg, ap);

    va_end(ap);

    exit(EXIT_FAILURE);
}

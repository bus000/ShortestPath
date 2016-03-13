#include "error.h"
#include <stdlib.h>
#include <stdio.h>

void usage(char const *program_name)
{
    fprintf(stderr, "usage\n");

    exit(EXIT_FAILURE);
}

void mem_err(void)
{
    fprintf(stderr, "out of memory\n");

    exit(EXIT_FAILURE);
}

void error_code(int error)
{
    fprintf(stderr, "error %d\n", error);

    exit(EXIT_FAILURE);
}

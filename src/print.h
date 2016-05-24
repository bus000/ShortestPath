#ifndef PRINT_H
#define PRINT_H

#include <stdio.h>

/* Print a vertex to the file f. */
void vertex_print(void *v, FILE *f);

/* Print a heap using the function print_el to print each pointer in the heap to
 * the file f. */
void heap_print(void *h, FILE *f, void (*print_el)(void *, FILE *));

#endif

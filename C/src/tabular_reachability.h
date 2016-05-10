#ifndef TABLE_REACHABILITY_H
#define TABLE_REACHABILITY_H

#include "graph.h"

typedef struct table_reachability_s {
    /* A vertex a reaches a vertex b if reaches[a][b] is true. */
    int8_t **reaches;

    /* Side length of the matrix of int8_t's. */
    int64_t side_len;
} table_reachability_t;

/* Construct a new reachability table by going through all vertices and writing
 * which other vertices they can reach. */
table_reachability_t table_init(digraph_t const *graph);

/* Find out if v1 reaches v2. */
int table_reaches(table_reachability_t const *table, vertex_t const *v1,
        vertex_t const *v2);

/* Free the resources used by the reachability table. */
void table_free(table_reachability_t *table);

#endif

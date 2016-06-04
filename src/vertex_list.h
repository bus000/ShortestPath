#ifndef VERTEX_LIST_H
#define VERTEX_LIST_H

typedef struct vertex_list_s {
    vertex_t **vertices;
    uint32_t len;
    uint32_t size;
} vertex_list_t;

/* Initializes a new vertex list, this function should be called before the
 * list is used. */
int vertex_list_init(vertex_list_t *list);

int vertex_list_init_array(vertex_list_t *list, vertex_t **vertices,
        size_t len);

int vertex_list_init_singular(vertex_list_t *list, vertex_t *vertex);

/* Returns true if the list contains a vertex with the given ID, false
 * otherwise. */
int vertex_list_contains(vertex_list_t const *list, vertex_id_t v);

/* Removes all elements from the list making it empty again. */
void vertex_list_empty(vertex_list_t *list);

/* Add a new vertex to a vertex list. */
int vertex_list_add(vertex_list_t *list, vertex_t *vertex);

/* Free all the resources used by the vertex list. */
void vertex_list_free(vertex_list_t *list);

uint32_t vertex_list_index_of(vertex_list_t const *list, vertex_id_t v);

uint32_t vertex_list_filter(vertex_list_t *list,
        int (*f)(vertex_t const *vertex));

#endif
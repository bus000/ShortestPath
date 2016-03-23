#ifndef LINKED_LIST_H
#define LINKED_LIST_H

#include <inttypes.h>

typedef struct ActualList {
    void const *element;
    struct ActualList *next;
    struct ActualList *prev;
} actual_list_t;

typedef struct {
    actual_list_t *start;
    actual_list_t *end;
    uint32_t len;
} linked_list_t;

/* Initialize a new empty linked list. */
int linked_list_init(linked_list_t *list);

/* Initialize a new list containing all the items in the array given. */
int linked_list_from_array(linked_list_t *list, void **items, uint32_t size);

int linked_list_add_end(linked_list_t *list, void *item);

/* Free the resources used by the list, does not call free on the pointers
 * contained in the list, that is the callers responsibility. */
void linked_list_free(linked_list_t *list);

#endif

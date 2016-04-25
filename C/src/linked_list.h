#ifndef LINKED_LIST_H
#define LINKED_LIST_H

#include <inttypes.h>

typedef struct ActualList {
    union {
        void const *element;
        uint32_t int_element;
    };
    struct ActualList *next;
    struct ActualList *prev;
} actual_list_t;

typedef struct {
    actual_list_t *start;
    actual_list_t *end;
    uint64_t len;
} linked_list_t;

/* Initialize a new empty linked list. */
int linked_list_init(linked_list_t *list);

/* Initialize a new list containing all the items in the array given. */
int linked_list_from_array(linked_list_t *list, void **items, uint32_t size);

/* Initialize a new linked list of length 1 with the single item given. */
int linked_list_singular(linked_list_t *list, void const *item);

/* Initialize a new list of integers of length 1 with the single item given. */
int linked_list_singular_int(linked_list_t *list, uint32_t item);

/* Add a new item to the end of the list. */
int linked_list_add_end(linked_list_t *list, void const *item);

/* Function to create a list of integers and not a list of pointers as usual. */
int linked_list_add_int_end(linked_list_t *list, uint32_t i);

/* Returns the pointers stored at the index'th position in the list. If index is
 * greater than 0 the list is traversed until reaching the i'th position, the
 * pointer here is returned. If index is less than 0, the list is traversed
 * backwards until the (list.len + index)'th position is reached. If index is
 * out the bounds of the list, the functions returns NULL. */
void const * linked_list_get(linked_list_t const *list, int64_t index);

/* Loop the list looking for the index given. If the index is negative the end
 * of the list is assumed, to be -1 and so on. When the correct index is found,
 * the element on that position is set to the element given. */
int linked_list_set(linked_list_t *list, int64_t index, void const *element);

/* Removes any pointer in the list comparing equal to the void pointer given.
 * Returns 0 if the element is found and -1 if the element is not found. */
int linked_list_remove(linked_list_t *list, void *item);

/* Free the resources used by the list, does not call free on the pointers
 * contained in the list, that is the callers responsibility. */
void linked_list_free(linked_list_t *list);

#endif

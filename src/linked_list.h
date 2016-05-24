#ifndef LINKED_LIST_H
#define LINKED_LIST_H

#include <inttypes.h>

typedef struct actual_list_s {
    union {
        void const *element;
        uint32_t int_element;
    };
    struct actual_list_s *next;
    struct actual_list_s *prev;
} actual_list_t;

typedef struct {
    actual_list_t *start;
    actual_list_t *end;
    uint64_t len;
} linked_list_t;

/* Initialize a new empty linked list. */
linked_list_t linked_list_init(void);

/* Initialize a new list containing all the items in the array given. */
linked_list_t linked_list_from_array(void **items, uint32_t size);

/* Initialize a new linked list of length 1 with the single item given. */
linked_list_t linked_list_singular(void const *item);

/* Initialize a new list of integers of length 1 with the single item given. */
linked_list_t linked_list_singular_int(uint32_t item);

/* Does a shallow copy of a linked list. All the pointers are copied and changes
 * to the old list does not affect the new list. The actual elements in the list
 * are not copied however and changes to the elements will change the list
 * elements. */
linked_list_t linked_list_copy(linked_list_t const *src);

/* Add a new item to the end of the list. */
int linked_list_add_end(linked_list_t *list, void const *item);

/* Add a new item to the start of the list. */
int linked_list_add_start(linked_list_t *list, void const *item);

/* Function to create a list of integers and not a list of pointers as usual. */
int linked_list_add_int_end(linked_list_t *list, uint32_t i);

/* Returns the pointers stored at the index'th position in the list. If index is
 * greater than 0 the list is traversed until reaching the i'th position, the
 * pointer here is returned. If index is less than 0, the list is traversed
 * backwards until the (list.len + index)'th position is reached. If index is
 * out the bounds of the list, the functions returns NULL. */
void const * linked_list_get(linked_list_t const *list, int64_t index);

/* Returns the uint32_t integers stored at the index'th position in the list.
 * If index is greater than 0 the list is traversed until reaching the i'th
 * position, the pointer here is returned. If index is less than 0, the list is
 * traversed backwards until the (list.len + index)'th position is reached. If
 * index is out the bounds of the list, the functions returns NULL. */
int linked_list_get_int(uint32_t *res, linked_list_t const *list,
        int64_t index);

/* Loop the list looking for the index given. If the index is negative the end
 * of the list is assumed, to be -1 and so on. When the correct index is found,
 * the element on that position is set to the element given. */
int linked_list_set(linked_list_t *list, int64_t index, void const *element);

/* Removes any pointer in the list comparing equal to the void pointer given.
 * Returns 0 if the element is found and -1 if the element is not found. */
int linked_list_remove(linked_list_t *list, void *item);

/* Remove the last element in the list, the function returns -1 if the list is
 * empty and the last element therefore cannot be removed and 0 on success. */
int linked_list_remove_last(linked_list_t *list);

int linked_list_remove_first(linked_list_t *list);

/* Add the whole list src to the end of the list dest. The pointers from the
 * list src are copied but the values they point to are not. */
int linked_list_concat(linked_list_t *dest, linked_list_t const *src);

/* Add the whole list src to the start of the list dest. The pointers from the
 * list src are copied but the values they point to are not. */
int linked_list_prepend(linked_list_t *dest, linked_list_t const *src);

/* Returns true if the list is empty and false otherwise. */
int linked_list_empty(linked_list_t const *list);

/* Free the resources used by the list, does not call free on the pointers
 * contained in the list, that is the callers responsibility. */
void linked_list_free(linked_list_t *list);

#endif

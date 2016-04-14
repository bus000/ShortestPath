#include <stdlib.h>
#include "linked_list.h"
#include "error.h"

int linked_list_init(linked_list_t *list)
{
    list->start = NULL;
    list->end = NULL;
    list->len = 0;

    return 0;
}

int linked_list_from_array(linked_list_t *list, void **items, uint32_t size)
{
    int i;
    int retcode;

    linked_list_init(list);

    /* Add all elements given to the list. */
    for (i = 0; i < size; i++) {
        retcode = linked_list_add_end(list, items[i]);

        if (retcode != 0) {
            linked_list_free(list);
            return retcode;
        }
    }

    return 0;
}

int linked_list_add_end(linked_list_t *list, void *item)
{
    actual_list_t *new_end;

    if (list->end == NULL) { /* First use of list. */
        list->start = list->end = malloc(sizeof(actual_list_t));
        if (list->start == NULL)
            mem_err();

        list->start->element = item;
        list->start->prev = NULL;
        list->start->next = NULL;
    } else { /* List has been used before. */
        new_end = malloc(sizeof(actual_list_t));
        if (new_end == NULL)
            mem_err();

        new_end->next = NULL;
        new_end->prev = list->end;
        new_end->element = item;
        list->end->next = new_end;
        list->end = new_end;
    }

    list->len += 1;

    return 0;
}

int linked_list_remove(linked_list_t *list, void *item)
{
    actual_list_t *element;

    for (element = list->start; element != NULL; element = element->next) {
        if (element->element == item) {
            element->prev->next = element->next;
            element->next->prev = element->prev;
            free(element);

            list->len -= 1;

            return 0;
        }
    }

    return -1;
}

void linked_list_free(linked_list_t *list)
{
    actual_list_t *element, *next_element;

    for (element = list->start; element != NULL; element = next_element) {
        next_element = element->next;
        free(element);
    }
}

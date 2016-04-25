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

int linked_list_singular(linked_list_t *list, void const *item)
{
    int retcode;

    if ((retcode = linked_list_init(list)) != 0)
        return retcode;

    if ((retcode = linked_list_add_end(list, item)) != 0)
        return retcode;

    return 0;
}

int linked_list_singular_int(linked_list_t *list, uint32_t item)
{
    int retcode;

    if ((retcode = linked_list_init(list)) != 0)
        return retcode;

    if ((retcode = linked_list_add_int_end(list, item)) != 0)
        return retcode;

    return 0;
}

int linked_list_add_end(linked_list_t *list, void const *item)
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

int linked_list_add_int_end(linked_list_t *list, uint32_t i)
{
    actual_list_t *new_end;

    if (list->end == NULL) { /* First use of list. */
        list->start = list->end = malloc(sizeof(actual_list_t));
        if (list->start == NULL)
            mem_err();

        list->start->int_element = i;
        list->start->prev = NULL;
        list->start->next = NULL;
    } else { /* List has been used before. */
        new_end = malloc(sizeof(actual_list_t));
        if (new_end == NULL)
            mem_err();

        new_end->next = NULL;
        new_end->prev = list->end;
        new_end->int_element = i;
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

int linked_list_remove_last(linked_list_t *list)
{
    actual_list_t *end;

    if (list->len == 0)
        return -1;

    end = list->end;
    list->end->prev->next = NULL;
    list->end = list->end->prev;

    free(end);

    return 0;
}

void const * linked_list_get(linked_list_t const *list, int64_t index)
{
    actual_list_t *alist;
    int64_t i;

    index = index < 0 ? list->len + index : index;

    if (index >= list->len || index < 0)
        return NULL;

    if (index > list->len / 2) {
        for (alist = list->end, i = list->len-1; i > index;
                alist = alist->prev, i--)
            ;
    } else {
        for (alist = list->start, i = 0; i < index; alist = alist->next, i++)
            ;
    }

    return alist->element;
}

int linked_list_set(linked_list_t *list, int64_t index, void const *element)
{
    actual_list_t *alist;
    int64_t i;

    index = index < 0 ? list->len + index : index;

    if (index >= list->len || index < 0)
        return -1;

    if (index > list->len / 2) {
        for (alist = list->end, i = list->len-1; i > index;
                alist = alist->prev, i--)
            ;
    } else {
        for (alist = list->start, i = 0; i < index; alist = alist->next, i++)
            ;
    }

    alist->element = element;

    return 0;
}

int linked_list_concat(linked_list_t *dest, linked_list_t const *src)
{
    int retcode;
    actual_list_t const *next;

    for (next = src->start; next != NULL; next = next->next) {
        retcode = linked_list_add_end(dest, next->element);

        if (retcode != 0)
            return retcode;
    }

    return 0;
}

void linked_list_free(linked_list_t *list)
{
    actual_list_t *element, *next_element;

    for (element = list->start; element != NULL; element = next_element) {
        next_element = element->next;
        free(element);
    }
}

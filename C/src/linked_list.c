#include <stdlib.h>
#include "linked_list.h"
#include "error.h"

static actual_list_t * list_goto(linked_list_t const *list, int64_t index)
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

    return alist;
}

linked_list_t linked_list_init(void)
{
    linked_list_t list;

    list.start = NULL;
    list.end = NULL;
    list.len = 0;

    return list;
}

linked_list_t linked_list_from_array(void **items, uint32_t size)
{
    int i, retcode;
    linked_list_t list = linked_list_init();

    /* Add all elements given to the list. */
    for (i = 0; i < size; i++) {
        retcode = linked_list_add_end(&list, items[i]);

        if (retcode != 0)
            linked_list_free(&list);
    }

    return list;
}

linked_list_t linked_list_singular(void const *item)
{
    linked_list_t list = linked_list_init();

    linked_list_add_end(&list, item);

    return list;
}

linked_list_t linked_list_singular_int(uint32_t item)
{
    linked_list_t list = linked_list_init();

    linked_list_add_int_end(&list, item);

    return list;
}

linked_list_t linked_list_copy(linked_list_t const *src)
{
    linked_list_t list = linked_list_init();
    actual_list_t *it;

    for (it = src->start; it != NULL; it = it->next)
        linked_list_add_end(&list, it->element);

    return list;
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

int linked_list_add_start(linked_list_t *list, void const *item)
{
    actual_list_t *new_start;

    if (list->start == NULL) { /* First use of list. */
        list->start = list->end = malloc(sizeof(actual_list_t));
        if (list->start == NULL)
            mem_err();

        list->start->element = item;
        list->start->prev = NULL;
        list->start->next = NULL;
    } else { /* List has been used before. */
        new_start = malloc(sizeof(actual_list_t));
        if (new_start == NULL)
            mem_err();

        new_start->next = list->start;
        new_start->prev = NULL;
        new_start->element = item;
        list->start->prev = new_start;
        list->start = new_start;
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

    if (list->len == 0) {
        return -1;
    } else if (list->len == 1) {
        free(list->end);
        list->start = NULL;
        list->end = NULL;

        list->len -= 1;
    } else {
        end = list->end;
        list->end->prev->next = NULL;
        list->end = list->end->prev;

        list->len -= 1;

        free(end);
    }

    return 0;
}

void const * linked_list_get(linked_list_t const *list, int64_t index)
{
    actual_list_t *alist = list_goto(list, index);

    return alist == NULL ? NULL : alist->element;
}

int linked_list_get_int(uint32_t *res, linked_list_t const *list,
        int64_t index)
{
    actual_list_t *alist = list_goto(list, index);

    if (alist == NULL) {
        return -1;
    } else {
        *res = alist->int_element;

        return 0;
    }
}

int linked_list_set(linked_list_t *list, int64_t index, void const *element)
{
    actual_list_t *alist = list_goto(list, index);

    if (alist == NULL) {
        return -1;
    } else {
        alist->element = element;

        return 0;
    }
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

int linked_list_prepend(linked_list_t *dest, linked_list_t const *src)
{
    int retcode;
    actual_list_t const *it;

    /* Traverse src backwards adding to start of dest. */
    for (it = src->end; it != NULL; it = it->prev) {
        retcode = linked_list_add_start(dest, it->element);

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

#include "../src/linked_list.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    uint32_t ints[100] = { 0 }, i;
    linked_list_t list = linked_list_init();
    linked_list_t start = linked_list_init();
    linked_list_t end = linked_list_init();
    actual_list_t *next;

    for (i = 0; i < 100; i++)
        ints[i] = i;

    /* Add 0 - 30 to start. */
    for (i = 0; i < 31; i++)
        linked_list_add_end(&start, &ints[i]);

    /* Add 31 - 77 to list. */
    for (i = 31; i < 78; i++)
        linked_list_add_end(&list, &ints[i]);

    /* Add 78 - 99 to end. */
    for (i = 78; i < 100; i++)
        linked_list_add_end(&end, &ints[i]);

    /* Prepend start to the beginning of list. */
    linked_list_prepend(&list, &start);

    /* Add end of list. */
    linked_list_concat(&list, &end);

    /* Print list. */
    for (next = list.start; next != NULL; next = next->next)
        printf("%u\n", *((uint32_t *) next->element));

    /* Free resources. */
    linked_list_free(&start);
    linked_list_free(&list);
    linked_list_free(&end);

    return EXIT_SUCCESS;
}

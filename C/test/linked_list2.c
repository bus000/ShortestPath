#include "../src/linked_list.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    linked_list_t list = linked_list_init();
    uint32_t n, n2;
    actual_list_t *alist;

    for (n = 0; n < 100; n += 10)
        linked_list_add_int_end(&list, n);

    for (alist = list.start; alist != NULL; alist = alist->next)
        printf("%u\n", alist->int_element);

    linked_list_free(&list);

    /* Make list with one element and get the last one. */
    list = linked_list_init();
    n = 1337;
    n2 = 123;
    linked_list_add_end(&list, &n);
    printf("last singular %u\n", *((uint32_t *) linked_list_get(&list, -1)));
    linked_list_add_end(&list, &n2);
    linked_list_add_end(&list, &n2);
    linked_list_remove_last(&list);
    linked_list_remove_last(&list);
    printf("last singular %u\n", *((uint32_t *) linked_list_get(&list, -1)));

    linked_list_free(&list);

    return EXIT_SUCCESS;
}

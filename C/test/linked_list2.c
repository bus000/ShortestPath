#include "../src/linked_list.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    linked_list_t list;
    uint32_t n;
    actual_list_t *alist;

    linked_list_init(&list);

    for (n = 0; n < 100; n += 10)
        linked_list_add_int_end(&list, n);

    for (alist = list.start; alist != NULL; alist = alist->next)
        printf("%u\n", alist->int_element);

    linked_list_free(&list);

    return EXIT_SUCCESS;
}

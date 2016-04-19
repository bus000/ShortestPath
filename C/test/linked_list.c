#include "../src/linked_list.h"
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char const *argv[])
{
    linked_list_t list;
    uint32_t ints[100] = { 0 }, i;

    linked_list_init(&list);

    for (i = 0; i < 100; i++) {
        ints[i] = i;

        linked_list_add_end(&list, &ints[i]);
    }

    printf("%u\n", *((uint32_t *) linked_list_get(&list, 0)));
    printf("%u\n", *((uint32_t *) linked_list_get(&list, 20)));
    printf("%u\n", *((uint32_t *) linked_list_get(&list, -1)));
    printf("%u\n", *((uint32_t *) linked_list_get(&list, -2)));
    printf("%u\n", *((uint32_t *) linked_list_get(&list, -55)));
    printf("%u\n", *((uint32_t *) linked_list_get(&list, 55)));
    printf("%p\n", linked_list_get(&list, 200));
    printf("%p\n", linked_list_get(&list, -101));

    linked_list_free(&list);

    return EXIT_SUCCESS;
}

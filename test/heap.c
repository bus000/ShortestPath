#include "../src/heap.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static int compare(void const *el1, void const *el2)
{
    uint32_t const *i1 = el1;
    uint32_t const *i2 = el2;

    if (*i1 < *i2)
        return -1;
    else if (*i1 > *i2)
        return 1;
    else
        return 0;
}

static void decrease_key(void *el, void *newkey)
{
    uint32_t *prev = el;
    uint32_t *new = newkey;

    *prev = *new;
}

int main(int argc, char const *argv[])
{
    uint32_t ints[56], *intsp[56], i, *prev, *cur;
    min_heap_t heap;

    srand(time(NULL));

    for (i = 0; i < 56; i++) {
        ints[i] = rand() % 200;
        intsp[i] = &ints[i];
    }

    heap_init(&heap, (void **) intsp, 56, compare, decrease_key);

    prev = heap_extract_min(&heap);
    while ((cur = heap_extract_min(&heap)) != NULL) {
        if (*cur < *prev)
            printf("error\n");

        prev = cur;
    }

    return EXIT_SUCCESS;
}

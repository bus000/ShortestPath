#include "../src/queue.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    queue_t queue;
    uint32_t ints[128] = { 0 }, i;

    for (i = 0; i < 128; i++)
        ints[i] = i;

    queue = queue_singular(&ints[100]);

    if (queue_empty(&queue))
        printf("queue is empty\n");
    printf("removing %u\n", *((uint32_t *) dequeue(&queue)));
    if (queue_empty(&queue))
        printf("queue is empty\n");

    enqueue(&queue, &ints[0]);
    enqueue(&queue, &ints[8]);
    enqueue(&queue, &ints[7]);
    enqueue(&queue, &ints[6]);
    enqueue(&queue, &ints[99]);
    enqueue(&queue, &ints[110]);
    enqueue(&queue, &ints[65]);
    enqueue(&queue, &ints[88]);
    enqueue(&queue, &ints[10]);

    printf("removing %u\n", *((uint32_t *) dequeue(&queue)));

    enqueue(&queue, &ints[110]);

    while (!queue_empty(&queue))
        printf("removing %u\n", *((uint32_t *) dequeue(&queue)));

    queue_free(&queue);

    return EXIT_SUCCESS;
}

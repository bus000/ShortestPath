#include "queue.h"
#include "linked_list.h"

queue_t queue_init(void)
{
    queue_t queue = { .queue = linked_list_init() };

    return queue;
}

queue_t queue_singular(void const *element)
{
    queue_t queue = queue_init();

    enqueue(&queue, element);

    return queue;
}

void enqueue(queue_t *queue, void const *element)
{
    linked_list_add_end(&queue->queue, element);
}

void const * dequeue(queue_t *queue)
{
    void const * element = linked_list_get(&queue->queue, 0);

    linked_list_remove_first(&queue->queue);

    return element;
}

int queue_empty(queue_t const *queue)
{
    return linked_list_empty(&queue->queue);
}

void queue_free(queue_t *queue)
{
    linked_list_free(&queue->queue);
}

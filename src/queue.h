#ifndef QUEUE_H
#define QUEUE_H

#include "linked_list.h"

/* Represents a FIFO queue. */
typedef struct queue_s {
    linked_list_t queue;
} queue_t;

/* Initialize a new FIFO queue. */
queue_t queue_init(void);

queue_t queue_singular(void const *element);

/* Add a new element to the back of the queue. */
void enqueue(queue_t *queue, void const *element);

/* Remove the top element from the queue.
 *
 * returns pointer to element - if queue not empty.
 * returns NULL otherwise. */
void const * dequeue(queue_t *queue);

int queue_empty(queue_t const *queue);

int64_t queue_size(queue_t const *queue);

void queue_free(queue_t *queue);

#endif

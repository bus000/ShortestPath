#ifndef STACK_H
#define STACK_H

#include <inttypes.h>
#include <stdlib.h>

typedef struct {
    void **stack;
    uint32_t top;
    uint32_t size;
} stack_t;

/* Create a new stack with the size given. */
stack_t stack_init(size_t size);

/* Put an item to the top of the stack. */
void stack_push(stack_t *stack, void *element);

/* Returns the element on the top of the stack or NULL if it does not exist. */
void * stack_pop(stack_t *stack);

/* Returns the element on the top of the stack and keeps the element there,
 * returns NULL if the stack is empty. */
void * stack_peek(stack_t const *stack);

/* Free the resources used by the stack. */
void stack_free(stack_t *stack);

#endif

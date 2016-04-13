#include "stack.h"
#include "error.h"
#include <stdlib.h>

stack_t stack_init(size_t size)
{
    stack_t stack;

    stack.stack = malloc(sizeof(void *) * size);
    stack.top = 0;
    stack.size = size;

    if (stack.stack == NULL)
        mem_err();

    return stack;
}

void stack_put(stack_t *stack, void *element)
{
    if (stack->top >= stack->size) {
        stack->size *= 2;
        stack->stack = realloc(stack->stack, sizeof(void *) * stack->size);

        if (stack->stack == NULL)
            mem_err();
    }

    stack->stack[stack->top] = element;
    stack->top += 1;
}

void * stack_pop(stack_t *stack)
{
    if (stack->top == 0) {
        return NULL;
    } else {
        stack->top -= 1;
        return stack->stack[stack->top];
    }
}

void * stack_peek(stack_t const *stack)
{
    return stack->top == 0 ? NULL : stack->stack[stack->top - 1];
}

void stack_free(stack_t *stack)
{
    if (stack->size != 0)
        free(stack->stack);
}

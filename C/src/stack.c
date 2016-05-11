#include "stack.h"
#include "error.h"
#include "mem_man.h"
#include <stdlib.h>

stack_t stack_init(size_t size)
{
    stack_t stack;

    MALLOC(stack.stack, sizeof(void *) * size);
    stack.top = 0;
    stack.size = size;

    return stack;
}

void stack_push(stack_t *stack, void const *element)
{
    if (stack->top >= stack->size) {
        stack->size *= 2;
        REALLOC(stack->stack, sizeof(void *) * stack->size);
    }

    stack->stack[stack->top] = element;
    stack->top += 1;
}

void const * stack_pop(stack_t *stack)
{
    if (stack->top == 0) {
        return NULL;
    } else {
        stack->top -= 1;
        return stack->stack[stack->top];
    }
}

void const * stack_peek(stack_t const *stack)
{
    return stack->top == 0 ? NULL : stack->stack[stack->top - 1];
}

int inline stack_empty(stack_t const *stack)
{
    return stack->top == 0;
}

void stack_free(stack_t *stack)
{
    if (stack->size != 0)
        free(stack->stack);
}

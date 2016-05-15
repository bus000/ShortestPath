#include "bin_tree.h"
#include "error.h"
#include <stdlib.h>
#include <stdio.h>

bin_tree_t bin_tree_init(cmp_t (*cmp_keys)(void const *el1, void const *el2))
{
    bin_tree_t tree = { .cmp_keys = cmp_keys, .tree = NULL };

    return tree;
}

int inline bin_tree_empty(bin_tree_t const *tree)
{
    return tree->tree == NULL;
}

int bin_tree_find(int64_t *res, bin_tree_t const *tree, void const *key)
{
    tree_container_t *next = tree->tree;

    while (next != NULL) {
        switch (tree->cmp_keys(next->key, key)) {
        case GT:
            next = next->left;
            break;
        case LT:
            next = next->right;
            break;
        case EQ:
            *res = next->value;
            return 0;
        }
    }

    return BIN_TREE_NOT_FOUND;
}

int bin_tree_insert(bin_tree_t *tree, void const *key, int64_t value)
{
    tree_container_t **next = &tree->tree, *parent = NULL;

    while (*next != NULL) {
        parent = *next;

        switch (tree->cmp_keys((*next)->key, key)) {
        case GT:
            next = &(*next)->left;
            break;
        case LT:
            next = &(*next)->right;
            break;
        case EQ:
            return BIN_TREE_CONFLICT;
        }
    }

    if ((*next = malloc(sizeof(tree_container_t))) == NULL)
        error_code(ERR_NO_MEM, "Out of memory\n");

    (*next)->key = key;
    (*next)->value = value;
    (*next)->left = NULL;
    (*next)->right = NULL;
    (*next)->parent = parent;

    return 0;
}

int bin_tree_remove(int64_t *oldval, bin_tree_t *tree, void const *key)
{
    tree_container_t *next = tree->tree, *target = NULL;
    cmp_t cmp;

    if (next == NULL)
        return BIN_TREE_NOT_FOUND;

    while (1) {
        cmp = tree->cmp_keys(next->key, key);

        if (cmp == EQ)
            target = next;

        if (cmp == GT) {
            if (next->left == NULL)
                break;

            next = next->left;
        } else {
            if (next->right == NULL)
                break;

            next = next->right;
        }
    }

    if (target == NULL)
        return BIN_TREE_NOT_FOUND;

    if (next->parent == NULL) {
        *oldval = next->value;
        free(next);
        tree->tree = NULL;
    } else {
        target->value = next->value;
        target->key = next->key;

        if (next->parent->left == next)
            next->parent->left = next->parent->right;
        else
            next->parent->right = next->parent->left;

        *oldval = next->value;
        free(next);
    }

    return 0;
}

int bin_tree_update(int64_t *oldval, bin_tree_t *tree, void const *key,
        int64_t newval)
{
    tree_container_t *next = tree->tree;

    while (next != NULL) {
        switch (tree->cmp_keys(next->key, key)) {
        case GT:
            next = next->left;
            break;
        case LT:
            next = next->right;
            break;
        case EQ:
            *oldval = next->value;
            next->value = newval;

            return 0;
        }
    }

    return BIN_TREE_NOT_FOUND;
}

static uint32_t bin_tree_size_help(tree_container_t const *cont)
{
    if (cont == NULL)
        return 0;

    return bin_tree_size_help(cont->left) + 1 + bin_tree_size_help(cont->right);
}

uint32_t bin_tree_size(bin_tree_t const *tree)
{
    return bin_tree_size_help(tree->tree);
}

static void bin_tree_print_help(tree_container_t const *cont)
{
    if (cont == NULL)
        return;

    bin_tree_print_help(cont->left);
    printf("%p - %" PRIu64 "\n", cont->key, cont->value);
    bin_tree_print_help(cont->right);
}

void bin_tree_print(bin_tree_t const *tree)
{
    bin_tree_print_help(tree->tree);
}

void bin_tree_free(bin_tree_t *tree)
{
    int64_t value;

    while (!bin_tree_empty(tree))
        bin_tree_remove(&value, tree, tree->tree->key);
}

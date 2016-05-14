#include "bin_tree.h"
#include "error.h"
#include <stdlib.h>

/* Find minimum node in a subtree. */
/*static tree_container_t * find_min(tree_container_t *current)*/
/*{*/
    /*while (current->left != NULL)*/
        /*current = current->left;*/

    /*return current;*/
/*}*/

/*static void replace_node_in_parent(tree_container_t *node,*/
        /*tree_container_t *val)*/
/*{*/
    /*if (node->parent != NULL)*/
        /*if (node->parent->left == node) {*/
            /*free(node->parent->left);*/
            /*node->parent->left = val;*/
        /*} else {*/
            /*free(node->parent->right);*/
            /*node->parent->right = val;*/
        /*}*/

    /*if (val != NULL)*/
        /*val->parent = node->parent;*/
/*}*/

bin_tree_t bin_tree_init(cmp_t (*cmp_keys)(void const *el1, void const *el2))
{
    bin_tree_t tree = { .cmp_keys = cmp_keys, .tree = NULL };

    return tree;
}

int inline bin_tree_empty(bin_tree_t const *tree)
{
    return tree->tree == NULL;
}

void const * bin_tree_find(bin_tree_t const *tree, void const *key)
{
    tree_container_t *next = tree->tree;

    while (tree != NULL) {
        switch (tree->cmp_keys(next->key, key)) {
        case GT:
            next = next->left;
            break;
        case LT:
            next = next->right;
            break;
        case EQ:
            return next->value;
        }
    }

    return NULL;
}

int bin_tree_insert(bin_tree_t *tree, void const *key, void const *value)
{
    tree_container_t **next = &tree->tree, *parent = NULL;

    while (*next != NULL) {
        switch (tree->cmp_keys((*next)->key, key)) {
        case GT:
            *next = (*next)->left;
            break;
        case LT:
            *next = (*next)->right;
            break;
        case EQ:
            return BIN_TREE_CONFLICT;
        }

        parent = *next;
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

int bin_tree_remove(void const *oldval, bin_tree_t *tree, void const *key)
{
    tree_container_t *next = tree->tree, *target = NULL;

    if (next == NULL)
        return BIN_TREE_NOT_FOUND;

    while (next != NULL) {
        switch (tree->cmp_keys(next->key, key)) {
        case GT:
            next = next->left;
            break;
        case LT:
            next = next->right;
            break;
        case EQ:
            target = next;
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

void bin_tree_free(bin_tree_t *tree)
{
    while (!bin_tree_empty(tree))
        bin_tree_remove(tree, tree->tree->key);
}

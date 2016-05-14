#ifndef BIN_TREE_H
#define BIN_TREE_H

#define BIN_TREE_CONFLICT  (1)
#define BIN_TREE_NOT_FOUND (2)

typedef enum cmp_e {
    GT, EQ, LT,
} cmp_t;

typedef struct tree_container_s {
    void const *key;
    void const *value;

    /* Everything to the left compares smaller than this element. */
    struct tree_container_s *left;

    /* Everything to the right compares greater than this element. */
    struct tree_container_s *right;

    /* Points to the immediate ancestor of this node. */
    struct tree_container_s *parent;
} tree_container_t;

typedef struct bin_tree_s {
    /* Actual container of the objects. */
    tree_container_t *tree;

    /* Function to compare keys in the tree. */
    cmp_t (*cmp_keys)(void const *el1, void const *el2);
} bin_tree_t;

/* Initialize a new binary tree, the comparison function should return GT when
 * the first key is greater than the second, LT if the first key is less than
 * the second and EQ otherwise. */
bin_tree_t bin_tree_init(cmp_t (*cmp_keys)(void const *el1, void const *el2));

/* Searches the binary tree for the value stored at the node in which the key
 * equals the key given.
 *
 * returns NULL    - If the value was not found.
 * returns pointer - Otherwise. */
void const * bin_tree_find(bin_tree_t const *tree, void const *key);

/* Insert a new value in the tree. The value is inserted in the place
 * corresponding to key.
 *
 * returns BIN_TREE_CONFLICT - If the key is already in the map.
 * returns 0                 - Otherwise. */
int bin_tree_insert(bin_tree_t *tree, void const *key, void const *value);

/* Find out if a tree is empty.
 *
 * returns true  - If the tree is empty.
 * returns false - Otherwise. */
int bin_tree_empty(bin_tree_t const *tree);

/* Removes a node from the binary tree, the value of the node removed are saved
 * to the oldval pointer.
 *
 * returns BIN_TREE_NOT_FOUND - If the key cannot be found in the tree.
 * returns 0                  - Otherwise. */
int bin_tree_remove(void const **oldval, bin_tree_t *tree, void const *key);

/* Free the resources used by a binary tree, does not free the key and value
 * pointers, that is the users responsibility. */
void bin_tree_free(bin_tree_t *tree);

#endif

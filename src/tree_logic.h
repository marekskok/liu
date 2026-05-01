#ifndef TREE_LOGIC_H
#define TREE_LOGIC_H
// How many children possible for one node
#define ORDER 33

typedef struct _node {
    bool is_leaf;
    int number_of_keys;
    struct _node *parent;

    // Sorted values in node
    int keys[ORDER - 1];

    // is_leaf == TRUE -> points to data frame row numbers
    // is_leaf == FALSE -> points to children nodes
    union {
        struct _node *children[ORDER]; // if is_leaf == false
        int row_indices[ORDER];       // if is_leaf == true
    } data;

    // If leaf:
    struct _node *prev_leaf;
    struct _node *next_leaf;
} node;

typedef struct _int_table {
    int* pointer;
    size_t size;
 } int_table;

void insert(node** root, int key, int row_index);

node* find_leaf(node* root, int key);

node* create_tree(int key, int row_index);

int_table find_indices(node* root, int key);

void free_tree(node* root);

int_table find_indices_interval(node* root, int start, int end);

int_table find_indices_min(node* root);

int_table find_indices_max(node* root);

#endif
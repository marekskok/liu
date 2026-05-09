// This file include all functions needed to build whole B+ tree with double keys
// I copied everything from build_int_tree.c and changed int to double
// B+ trees structure: https://www.youtube.com/watch?v=o_2psWN8k_c
#include<stdlib.h>
#include<stdbool.h>
#include"declarations.h"

double_node* create_node_double(bool is_leaf) {
    // Function creates pointer to an empty double_node without connection
    double_node* new_double_node = malloc(sizeof(double_node));
    new_double_node->is_leaf = is_leaf;
    new_double_node->number_of_keys = 0;
    new_double_node->next_leaf = NULL;
    new_double_node->prev_leaf = NULL;
    new_double_node->parent = NULL;
    for (size_t i = 0; i < ORDER; i++) {
        if (is_leaf) new_double_node->data.row_indices[i] = -1;
        else new_double_node->data.children[i] = NULL;
    }
    for (size_t i = 0; i < ORDER - 1; i++) {
        new_double_node->keys[i] = 0;
    }
    return new_double_node;
}

double_node* find_leaf_double(double_node* root, double key) {
    // Function returns pointer to the first double_node where key value would fit in
    if (root == NULL) {
        return root;
    }

    double_node* cursor = root;
    // Go down until it is leaf 
    while (!cursor->is_leaf) {
        size_t i = 0;

        // Chose children based on where key is <=
        while (i < cursor->number_of_keys) {
            if (key <= cursor->keys[i]) {
                break;
            }
            i++;
        }
        cursor = (double_node*)cursor->data.children[i];
    }
    return cursor;
}

double_node* find_leaf_last_double(double_node* root, double key) {
    // Function returns pointer to the last double_node where key value would fit in
    if (root == NULL) return NULL;

    double_node* cursor = root;

    while (!cursor->is_leaf) {
        size_t i = 0;

        // Differs from previous function < instead of <=
        while (i < cursor->number_of_keys) {
            if (key < cursor->keys[i]) {
                break;
            }
            i++;
        }
        cursor = (double_node*)cursor->data.children[i];
    }
    return cursor;
}

void insert_into_leaf_double(double_node* leaf, double key, int row_index) {
    // Function inserts key value and row index into the given double_node keys list 
    // on the condition that there is space

    size_t insertion_point = 0;

    // Looks for correct position for new key
    while (insertion_point < leaf->number_of_keys && leaf->keys[insertion_point] <= key) {
        insertion_point++;
    }

    // Shifts existing keys and pointers to the right
    for (size_t i = leaf->number_of_keys; i > insertion_point; i--) {
        leaf->keys[i] = leaf->keys[i - 1];
        leaf->data.row_indices[i] = leaf->data.row_indices[i - 1];
    }

    // Insert new key and index number as his pointer
    leaf->keys[insertion_point] = key;
    leaf->data.row_indices[insertion_point] = row_index;
    leaf->number_of_keys++;
}

double_node* insert_into_new_root_double(double_node* left, double key, double_node* right) {
    // This function is needed when there is only one full leaf, which is also root
    // then after spliting we need to create new root with splitted half children
    double_node* root = create_node_double(false);
    root->keys[0] = key;
    root->data.children[0] = left;
    root->data.children[1] = right;
    root->number_of_keys++;
    left->parent = root;
    right->parent = root;
    return root;
}

void insert_into_double_node_double(double_node* parent, int left_index, double key, double_node* right) {
    // This function is used when there is space in double_node which isn't leaf
    for (size_t i = parent->number_of_keys; i > left_index; i--) {
        parent->data.children[i + 1] = parent->data.children[i];
        parent->keys[i] = parent->keys[i - 1];
    }
    parent->keys[left_index] = key;
    parent->data.children[left_index + 1] = right;
    parent->number_of_keys++;
    right->parent = parent;
}

void insert_into_parent_double(double_node** root, double_node* left, double key, double_node* right);

void split_and_insert_leaf_double(double_node** root, double_node* leaf, double key, int row_index) {
    // This function is used when there isn't space for new key in double_node where it shoud be inserted
    // It creates new double_node(leaf) and put there half of old one

    double_node* new_leaf = create_node_double(true);
    new_leaf->parent = leaf->parent;
    
    // Temporary tables for all keys and row_indices including new one
    double temp_keys[ORDER];
    int temp_indices[ORDER];
    size_t i, j, split_point;

    i = 0;
    while (i < ORDER - 1 && leaf->keys[i] <= key) i++;

    for (j = 0; j < ORDER - 1; j++) {
        if (j >= i) {
            temp_keys[j + 1] = leaf->keys[j];
            temp_indices[j + 1] = leaf->data.row_indices[j];
        } else {
            temp_keys[j] = leaf->keys[j];
            temp_indices[j] = leaf->data.row_indices[j];
        }
    }
    temp_keys[i] = key;
    temp_indices[i] = row_index;

    if (ORDER % 2 == 0) split_point = ORDER / 2;
    else split_point = ORDER / 2 + 1;

    // Cleaning first leaf and inserting first half of data
    leaf->number_of_keys = 0;
    for (i = 0; i < split_point; i++) {
        leaf->keys[i] = temp_keys[i];
        leaf->data.row_indices[i] = temp_indices[i];
        leaf->number_of_keys++;
    }

    // Second leaf with second half of data
    for (i = split_point, j = 0; i < ORDER; i++, j++) {
        new_leaf->keys[j] = temp_keys[i];
        new_leaf->data.row_indices[j] = temp_indices[i];
        new_leaf->number_of_keys++;
    }

    // Changing two-way list of leafs
    new_leaf->next_leaf = leaf->next_leaf;
    if (new_leaf->next_leaf != NULL) {
        new_leaf->next_leaf->prev_leaf = new_leaf;
    }
    leaf->next_leaf = new_leaf;
    new_leaf->prev_leaf = leaf;

    // Copy of the smallest key of new leaf goes up to the parent
    double parent_key = new_leaf->keys[0];
    insert_into_parent_double(root, leaf, parent_key, new_leaf);
}

void split_and_insert_internal_double(double_node** root, double_node* old_double_node, int left_index, double key, double_node* right) {
    // This function is used when there isn't space in parent double_node
    double_node* new_double_node = create_node_double(false);

    size_t i, j, split_point;
    double temp_keys[ORDER];
    double_node* temp_children[ORDER + 1];

    // Copying data to temporary tables (could be one loop but complicated)
    for (i = 0, j = 0; i < old_double_node->number_of_keys + 1; i++, j++) {
        if (j == left_index + 1) j++;
        temp_children[j] = old_double_node->data.children[i];
    }
    for (i = 0, j = 0; i < old_double_node->number_of_keys; i++, j++) {
        if (j == left_index) j++;
        temp_keys[j] = old_double_node->keys[i];
    }

    temp_keys[left_index] = key;
    temp_children[left_index + 1] = right;

    if (ORDER % 2 == 0) split_point = ORDER / 2;
    else split_point = ORDER / 2 + 1;
    old_double_node->number_of_keys = 0;
    new_double_node->parent = old_double_node->parent;

    // Inserting first half of data to old double_node
    for (i = 0; i < split_point - 1; i++) {
        old_double_node->data.children[i] = temp_children[i];
        old_double_node->keys[i] = temp_keys[i];
        old_double_node->number_of_keys++;
    }
    old_double_node->data.children[i] = temp_children[i];

    double k_prime = temp_keys[split_point - 1];

    int k = i + 1; 
    for (j = 0; k < ORDER; k++, j++) {
        new_double_node->data.children[j] = temp_children[k];
        new_double_node->keys[j] = temp_keys[k];
        new_double_node->number_of_keys++;
    }
    new_double_node->data.children[j] = temp_children[k];

    // Changing parent in moved double_nodes
    for (i = 0; i <= new_double_node->number_of_keys; i++) {
        (new_double_node->data.children[i])->parent = new_double_node;
    }
    // Recursive inerstion
    insert_into_parent_double(root, old_double_node, k_prime, new_double_node);
}

void insert_into_parent_double(double_node** root, double_node* left, double key, double_node* right) {
    double_node* parent = left->parent;

    // If leaf was also root
    if (parent == NULL) {
        *root = insert_into_new_root_double(left, key, right);
        left->parent = *root;
        right->parent = *root;
        return;
    }

    // Index of left leaf in parent double_node
    size_t left_index = 0;
    while (left_index <= parent->number_of_keys && parent->data.children[left_index] != left) {
        left_index++;
    }

    // If there is space in parent root
    if (parent->number_of_keys < ORDER - 1) {
        insert_into_double_node_double(parent, left_index, key, right);
    } 
    // If there isn't space in parent root
    else {
        split_and_insert_internal_double(root, parent, left_index, key, right);
    }
}

void insert_double(double_node** root, double key, int row_index) {
    // Creates new root if tree is empty
    if (*root == NULL) {
        *root = create_node_double(true);
        (*root)->keys[0] = key;
        (*root)->data.row_indices[0] = row_index;
        (*root)->number_of_keys++;
        (*root)->parent = NULL;
        return;
    }

    // Find proper leaf using function find_leaf
    double_node* leaf = find_leaf_last_double(*root, key);

    // Check if there is space in leaf
    if (leaf->number_of_keys < ORDER - 1) {
        insert_into_leaf_double(leaf, key, row_index);
    } else {
        split_and_insert_leaf_double(root, leaf, key, row_index);
    }
}

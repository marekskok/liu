// Int functions from this file are used by R API functions
#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
#include"declarations.h"

int_table find_indices_int(int_node* root, int key){
    // Looks for row indices of elements in leafs where keys are the same
    // It returns struct which is basically int vector

    // Empty table
    int_table t = {NULL, 0};
    size_t size = 0;
    int flag = 0;
    if (root == NULL) return t;

    // Uses function from build_int_tree.c to find leaf
    int_node* start_leaf = find_leaf_int(root, key);
    int_node* leaf = start_leaf;

    // Counting how much memory is needed
    while(leaf){
        for (size_t i=0; i<leaf->number_of_keys; i++){
            if (leaf->keys[i] == key) size++;
            if (leaf->keys[i] > key) {
                flag = 1;
                break;
            }
        }
        if (flag) break;
        leaf = leaf->next_leaf;
    }

    if (size == 0) return t;

    t.size = size;
    t.pointer = malloc(t.size*sizeof(int));
    size_t j = 0;

    // Saving values to the table
    leaf = start_leaf;
    flag = 0;
    while(leaf){
        for (size_t i=0; i<leaf->number_of_keys; i++){
            if (leaf->keys[i] == key) t.pointer[j++] = leaf->data.row_indices[i];
            if (leaf->keys[i] > key) {
                flag = 1;
                break;
            }
        }
        if (flag) break;
        leaf = leaf->next_leaf;
    }

    return t;
}

int_table find_indices_interval_int(int_node* root, int start, int end){
    // Looks for row indices of elements in leafs where keys are between given interval
    // It returns struct which is basically int vector

    // Empty table
    int_table t = {NULL, 0};
    if (root == NULL) return t;

    // When I am writing this comment I have no idea why it doesn't use find_leaf_int() function
    // but since it works properly I am afraid to change it
    int_node* curr = root;
    while(!curr->is_leaf){
        size_t i = 0;
        while (i < curr->number_of_keys && start > curr->keys[i]) {
            i++;
        }
        curr = curr->data.children[i];
    }
    int_node* begining = curr;

    // Counting how much memory is needed
    int flag = 0;
    while(curr){
        for (size_t i=0; i<curr->number_of_keys; i++){
            if (curr->keys[i] >= start && curr->keys[i] < end) t.size++;
            if (curr->keys[i] >= end) {
                flag = 1;
                break;
            }
        }
        if (flag) break;
        curr = curr->next_leaf;
    }
    if (t.size == 0) return t;

    t.pointer = malloc(t.size*sizeof(int));

    // Saving values to the table
    curr = begining;
    flag = 0;
    size_t j = 0;
    while(curr){
        for (size_t i=0; i<curr->number_of_keys; i++){
            if (curr->keys[i] >= start && curr->keys[i] < end) t.pointer[j++] = curr->data.row_indices[i];
            if (curr->keys[i] >= end) {
                flag = 1;
                break;
            }
        }
        if (flag) break;
        curr = curr->next_leaf;
    }
    return t;
}

void free_tree_int(int_node* root) {
    // Recursive function needed to free memory
    if (root == NULL) return;

    if (!root->is_leaf) {
        for (int i = 0; i <= root->number_of_keys; i++) {
            free_tree_int((int_node*)root->data.children[i]);
        }
    }
    free(root);
}

int_table find_indices_min_int(int_node* root){
    // It really does what it says

    // Empty table
    int_table t = {NULL, 0};
    if (root == NULL) return t;

    int_node* curr = root;

    // Going to the left
    while(!curr->is_leaf){
        curr = curr->data.children[0];
    }
    if (curr == NULL || curr->number_of_keys == 0) return t;

    // Saving most left node
    int_node* first_leaf = curr;
    int min_value = curr->keys[0];
    t.size = 1;

    // Then we go right until we find diffrent key and count it
    int flag = 0;
    while(curr){
        for (size_t i=1; i<curr->number_of_keys; i++){
            if(curr->keys[i] == min_value){
                t.size++;
            } else {
                flag = 1;
                break;
            }
        }
        if (flag) break;
        curr = curr->next_leaf;
    }
    t.pointer = malloc(t.size*sizeof(int));

    // Now saving part
    curr = first_leaf;
    flag = 0;
    size_t j = 0;
    while(curr){
        for (size_t i=0; i<curr->number_of_keys; i++){
            if(curr->keys[i] == min_value){
                t.pointer[j++] = curr->data.row_indices[i];
            } else {
                flag = 1;
                break;
            }
        }
        if (flag) break;
        curr = curr->next_leaf;
    }
    return t;
}

int_table find_indices_max_int(int_node* root){
    // Same here

    // Empty table
    int_table t = {NULL, 0};
    if (root == NULL) return t;
    int_node* curr = root;

    // Now we go to the right
    while (!curr->is_leaf){
        curr = curr->data.children[curr->number_of_keys];
    }
    if (curr == NULL || curr->number_of_keys == 0) return t;

    // Saving last node
    int_node* last_leaf = curr;

    // Getting max value and counting keys
    int max_value = curr->keys[curr->number_of_keys-1];
    int flag = 0;
    while(curr){
        for (int i=curr->number_of_keys-1; i >= 0; i--){
            if (curr->keys[i] == max_value) {t.size++;
            } else {
                flag = 1;
                break;
            }
        }
            if (flag) break;
            curr = curr->prev_leaf;
    }

    t.pointer = malloc(t.size*sizeof(int));

    // Saving
    curr = last_leaf;
    flag = 0;
    int j=t.size-1;
    while(curr){
        for (int i=curr->number_of_keys-1; i >= 0; i--){
            if (curr->keys[i] == max_value) {t.pointer[j--] = curr->data.row_indices[i];
             } else {
                flag = 1;
                break;
            }
        }
        if (flag) break;
        curr = curr->prev_leaf;
    }
    return t;
}

dual_int_table inner_join_int(int_table v, int_node* root){
    // This function might need some comment
    // As it says it is inner join between v indices and row indices from root
    // based on v values and tree keys
    // Example:
    // v.pointer (Left Table IDs):  [10, 20, 30]  (indices: 0, 1, 2)
    // B+Tree (Right Table):       {Key 20: Row 5, Key 20: Row 8, Key 30: Row 12}
    // 
    // Result (dual_int_table):
    // left_indices:  [1, 1, 2]     (v indices + 1 for R compatibility)
    // right_indices: [5, 8, 12]    (row indices found in tree)
    // but put into double_int_table

    // Empty structure
    dual_int_table res;
    res.size = 0;
    res.capacity = v.size;
    res.left_indices = malloc(res.capacity*sizeof(int));
    res.right_indices = malloc(res.capacity*sizeof(int));

    // For every value in v we look for matches
    for (size_t i = 0; i < v.size; i++) {
        // Using function from earlier
        int_table matches = find_indices_int(root, v.pointer[i]);
        
        // Note that if there are no matches we don't chose it becaues it is inner
        if (matches.size > 0) {
            // Changing capacity if needed
            if (res.size + matches.size > res.capacity) {
                res.capacity = (res.size + matches.size) * 2;
                res.left_indices = realloc(res.left_indices, res.capacity*sizeof(int));
                res.right_indices = realloc(res.right_indices, res.capacity*sizeof(int));
            }
            // Saving
            for (size_t m = 0; m < matches.size; m++) {
                res.right_indices[res.size] = matches.pointer[m];
                res.left_indices[res.size] = i + 1;
                res.size++;
            }
            free(matches.pointer);
        }
    }
    return res;
}



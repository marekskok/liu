#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
#include"tree_logic.h"

int_table find_indices(node* root, int key){
    int_table t = {NULL, 0};
    size_t size = 0;
    int flag = 0;
    if (root == NULL) return t;

    node* start_leaf = find_leaf(root, key);
    node* leaf = start_leaf;

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

int_table find_indices_interval(node* root, int start, int end){
    int_table t = {NULL, 0};
    if (root == NULL) return t;

    node* curr = root;
    while(!curr->is_leaf){
        size_t i = 0;
        while (i < curr->number_of_keys && start > curr->keys[i]) {
            i++;
        }
        curr = curr->data.children[i];
    }
    node* begining = curr;

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

void free_tree(node* root) {
    if (root == NULL) return;

    if (!root->is_leaf) {
        for (int i = 0; i <= root->number_of_keys; i++) {
            free_tree((node*)root->data.children[i]);
        }
    }
    free(root);
}

int_table find_indices_min(node* root){
    int_table t = {NULL, 0};
    if (root == NULL) return t;

    node* curr = root;

    while(!curr->is_leaf){
        curr = curr->data.children[0];
    }
    if (curr == NULL || curr->number_of_keys == 0) return t;


    node* first_leaf = curr;
    int min_value = curr->keys[0];
    t.size = 1;

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

int_table find_indices_max(node* root){
    int_table t = {NULL, 0};
    if (root == NULL) return t;
    node* curr = root;

    while (!curr->is_leaf){
        curr = curr->data.children[curr->number_of_keys];
    }
    if (curr == NULL || curr->number_of_keys == 0) return t;

    node* last_leaf = curr;

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





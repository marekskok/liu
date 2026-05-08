#ifndef TREE_LOGIC_H
#define TREE_LOGIC_H

// How many children possible for one node
#define ORDER 33

// STRUCTS:
// int:
// Most important struct, contains int B+tree nodes
typedef struct _int_node {
    bool is_leaf;
    size_t number_of_keys;
    struct _int_node *parent;

    // Sorted values in node
    int keys[ORDER - 1];

    // is_leaf == TRUE -> points to data frame row numbers
    // is_leaf == FALSE -> points to children nodes
    union {
        struct _int_node *children[ORDER];
        int row_indices[ORDER];
    } data;

    // If leaf:
    struct _int_node *prev_leaf;
    struct _int_node *next_leaf;
} int_node;
// Used to trasfer int vectors between functions
typedef struct _int_table {
    int* pointer;
    size_t size;
 } int_table;
// Used to trasfer double int vectors between functions (only in inner join)
typedef struct _dual_int_table {
    int* left_indices;
    int* right_indices;
    size_t size;
    size_t capacity;
} dual_int_table;

// double:
// Most important struct, contains double B+tree nodes
typedef struct _double_node {
    bool is_leaf;
    size_t number_of_keys;
    struct _double_node *parent;

    // Sorted values in node
    double keys[ORDER - 1];

    // is_leaf == TRUE -> points to data frame row numbers
    // is_leaf == FALSE -> points to children nodes
    union {
        struct _double_node *children[ORDER];
        int row_indices[ORDER];
    } data;

    // If leaf:
    struct _double_node *prev_leaf;
    struct _double_node *next_leaf;
} double_node;
typedef struct _double_table {
    double* pointer;
    size_t size;
 } double_table;


// Int functions:
// Used for building tree
void insert_int(int_node** root, int key, int row_index);

// Used by R API functions
int_node* find_leaf_int(int_node* root, int key);
void find_indices_int(int_node* root, int key, int_table* result);
int_table find_indices_interval_int(int_node* root, int start, int end);
int_table find_indices_min_int(int_node* root, int* min);
int_table find_indices_max_int(int_node* root, int* max);
dual_int_table* inner_join_int(int_table keys, int_node* root, bool left, size_t* out_size);
void free_tree_int(int_node* root);

// Double functions:
// Used for building tree
void insert_double(double_node** root, double key, int row_index);

// Used by R API functions
double_node* find_leaf_double(double_node* root, double key);
void find_indices_double(double_node* root, double key, int_table* result);
int_table find_indices_interval_double(double_node* root, double start, double end);
int_table find_indices_min_double(double_node* root, double* min);
int_table find_indices_max_double(double_node* root, double* max);
dual_int_table* inner_join_double(double_table keys, double_node* root, bool left, size_t* out_size);
void free_tree_double(double_node* root);

#endif
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include "tree_logic.h"
#include <R_ext/Print.h>

SEXP r_build_tree_from_df(SEXP df, SEXP col_name) {
    if (!Rf_isNewList(df) || !Rf_inherits(df, "data.frame")) {
        Rf_error("Object must be data.frame");
    }

    // Reading column name as char
    SEXP names = Rf_getAttrib(df, R_NamesSymbol);
    const char* target_col = CHAR(Rf_asChar(col_name));

    // Finding index of column
    int col_idx = -1;
    int n_cols = (int)Rf_xlength(df);

    for (int i = 0; i < n_cols; i++) {
        if (strcmp(CHAR(STRING_ELT(names, i)), target_col) == 0) {
            col_idx = i;
            break;
        }
    }

    if (col_idx == -1) {
        Rf_error("Didn't find column named: %s", target_col);
    }

    // Saving column of int
    SEXP column = VECTOR_ELT(df, col_idx);
    if (!Rf_isInteger(column)) {
        Rf_error("Kolumna %s musi zawierać liczby całkowite (integer)", target_col);
    }

    int *data_ptr = INTEGER(column);
    int n = Rf_length(column);


    node *root = NULL; 
    for (int i = 0; i < n; i++) {
        insert(&root, data_ptr[i], i+1);
    }

    SEXP root_ptr = Rf_protect(R_MakeExternalPtr(root, Rf_install("BTree_Class"), R_NilValue));
    Rf_unprotect(1);
    return root_ptr;
}

SEXP r_search_by_key(SEXP btree_ptr, SEXP key_to_find) {
    if (TYPEOF(btree_ptr) != EXTPTRSXP) {
        Rf_error("Argument must be an External Pointer (BTree_Class)");
    }

    if (R_ExternalPtrTag(btree_ptr) != Rf_install("BTree_Class")) {
    Rf_error("Provided pointer is not a BTree_Class object");
    }

    // Getting root
    node* root = (node*)R_ExternalPtrAddr(btree_ptr);
    
    if (root == NULL) {
        Rf_error("Tree pointer is NULL (empty or corrupted)");
    }

    // Converting key to int
    int key = Rf_asInteger(key_to_find);

    int_table table = find_indices(root, key);

    SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

    memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));

    free(table.pointer);
    Rf_unprotect(1);
    return indices;
}

SEXP btree_free(SEXP btree_ptr){
    if (TYPEOF(btree_ptr) != EXTPTRSXP) {
        Rf_error("Argument must be an External Pointer (BTree_Class)");
    }

    if (R_ExternalPtrTag(btree_ptr) != Rf_install("BTree_Class")) {
    Rf_error("Provided pointer is not a BTree_Class object");
    }

    // Getting root
    node* root = (node*)R_ExternalPtrAddr(btree_ptr);

    free_tree(root);
    R_ClearExternalPtr(btree_ptr);
    Rprintf("B+Tree memory freed.\n");

    return R_NilValue;
}

SEXP r_search_by_interval(SEXP btree_ptr, SEXP start, SEXP end) {
    if (TYPEOF(btree_ptr) != EXTPTRSXP) {
        Rf_error("Argument must be an External Pointer (BTree_Class)");
    }

    if (R_ExternalPtrTag(btree_ptr) != Rf_install("BTree_Class")) {
    Rf_error("Provided pointer is not a BTree_Class object");
    }

    // Getting root
    node* root = (node*)R_ExternalPtrAddr(btree_ptr);
    
    if (root == NULL) {
        Rf_error("Tree pointer is NULL (empty or corrupted)");
    }

    // Converting key to int
    int start1 = Rf_asInteger(start);
    int end1 = Rf_asInteger(end);

    int_table table = find_indices_interval(root, start1, end1);

    SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

    memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));

    free(table.pointer);
    Rf_unprotect(1);
    return indices;
}

SEXP r_search_min(SEXP btree_ptr) {
    if (TYPEOF(btree_ptr) != EXTPTRSXP) {
        Rf_error("Argument must be an External Pointer (BTree_Class)");
    }

    if (R_ExternalPtrTag(btree_ptr) != Rf_install("BTree_Class")) {
    Rf_error("Provided pointer is not a BTree_Class object");
    }

    // Getting root
    node* root = (node*)R_ExternalPtrAddr(btree_ptr);
    
    if (root == NULL) {
        Rf_error("Tree pointer is NULL (empty or corrupted)");
    }

    int_table table = find_indices_min(root);
    SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

    memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));

    free(table.pointer);
    Rf_unprotect(1);
    return indices;
}

SEXP r_search_max(SEXP btree_ptr) {
    if (TYPEOF(btree_ptr) != EXTPTRSXP) {
        Rf_error("Argument must be an External Pointer (BTree_Class)");
    }

    if (R_ExternalPtrTag(btree_ptr) != Rf_install("BTree_Class")) {
    Rf_error("Provided pointer is not a BTree_Class object");
    }

    // Getting root
    node* root = (node*)R_ExternalPtrAddr(btree_ptr);
    
    if (root == NULL) {
        Rf_error("Tree pointer is NULL (empty or corrupted)");
    }

    int_table table = find_indices_max(root);
    SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

    memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));

    free(table.pointer);
    Rf_unprotect(1);
    return indices;
}




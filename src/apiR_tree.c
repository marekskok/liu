// This file is translator between liu_interface.R nad functions_tree
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include "declarations.h"

void r_index_free(SEXP index_ptr) {
    // This function recognizes index type and uses right free function

    if (TYPEOF(index_ptr) != EXTPTRSXP) return;

    if (R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_int")) {
        // Getting root
        int_node* root = (int_node*)R_ExternalPtrAddr(index_ptr);
        if (root == NULL) return;

        free_tree_int(root);
        R_ClearExternalPtr(index_ptr);
        return;
    } else if (R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_double")) {
        // Getting root
        double_node* root = (double_node*)R_ExternalPtrAddr(index_ptr);
        if (root == NULL) return;

        free_tree_double(root);
        R_ClearExternalPtr(index_ptr);
        return;
    } else {
    Rf_error("Provided pointer is not a LIU index object");
    }
}

SEXP r_build_tree_from_df(SEXP df, SEXP col_name) {
    // This function doesn't validate arguments because R file did it.
    // It translate R objects and build C tree.

    // Reading column names as char
    SEXP names = Rf_getAttrib(df, R_NamesSymbol);
    const char* target_col = CHAR(Rf_asChar(col_name));

    // Finding index of column
    int col_idx = -1;
    int n_cols = (int)LENGTH(df);

    for (int i = 0; i < n_cols; i++) {
        if (strcmp(CHAR(STRING_ELT(names, i)), target_col) == 0) {
            col_idx = i;
            break;
        }
    }
    if (col_idx == -1) {
        Rf_error("Didn't find column");
    }

    // Saving column
    SEXP column = VECTOR_ELT(df, col_idx);
    if (!Rf_isInteger(column) && !Rf_isReal(column)) {
    Rf_error("Column must be integer or double");
    }

    // Case when int
    if (Rf_isInteger(column)){
        int *data_ptr = INTEGER(column);
        size_t n = Rf_length(column);

        int_node *root = NULL; 
        for (size_t i = 0; i < n; i++) {
            if (data_ptr[i] != NA_INTEGER) {
                insert_int(&root, data_ptr[i], i + 1);
            }
        }

        SEXP root_ptr = PROTECT(R_MakeExternalPtr(root, Rf_install("liu_index_int"), R_NilValue));
        Rf_setAttrib(root_ptr, R_ClassSymbol, Rf_mkString("liu_index"));
        R_RegisterCFinalizerEx(root_ptr, r_index_free, TRUE);
        UNPROTECT(1);
        return root_ptr;
    } else { // when double
        double *data_ptr = REAL(column);
        size_t n = Rf_length(column);

        double_node *root = NULL; 
        for (size_t i = 0; i < n; i++) {
            if (data_ptr[i] != NA_REAL){
                insert_double(&root, data_ptr[i], i+1);
            }
        }

        SEXP root_ptr = PROTECT(R_MakeExternalPtr(root, Rf_install("liu_index_double"), R_NilValue));
        Rf_setAttrib(root_ptr, R_ClassSymbol, Rf_mkString("liu_index"));
        R_RegisterCFinalizerEx(root_ptr, r_index_free, TRUE);
        UNPROTECT(1);
        return root_ptr;
    }
}

SEXP r_search_by_key(SEXP index_ptr, SEXP keys_to_find) {
    // Checking for index class and calling right function int or double

    if (TYPEOF(index_ptr) != EXTPTRSXP) {
        Rf_error("Argument must be an External Pointer (liu_pointer)");
    }

    if (R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_int")) {
        if (TYPEOF(keys_to_find) != INTSXP) {
            Rf_error("Argument keys must be same type as liu pointer (int)");
        }
        // Getting root
        int_node* root = (int_node*)R_ExternalPtrAddr(index_ptr);
        
        if (root == NULL) Rf_error("Tree pointer is NULL (empty or corrupted)");

        // Converting key to int
        int_table keys;
        keys.size = LENGTH(keys_to_find);

        keys.pointer = INTEGER(keys_to_find);

        int_table table = {NULL, 0};
        for (size_t i = 0; i<keys.size; i++){
            find_indices_int(root, keys.pointer[i], &table);
        }

        SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

        memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));

        free(table.pointer);
        Rf_unprotect(1);
        return indices;
    } else if (R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_double")){
        // Getting root
        double_node* root = (double_node*)R_ExternalPtrAddr(index_ptr);

        if (root == NULL) Rf_error("Tree pointer is NULL (empty or corrupted)");

        // Converting key to int
        double_table keys;
        keys.size = LENGTH(keys_to_find);

        keys.pointer = REAL(keys_to_find);
        int_table table = {NULL, 0};

        for (size_t i = 0; i<keys.size; i++){
            find_indices_double(root, keys.pointer[i], &table);
        }

        SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

        memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));

        free(table.pointer);
        Rf_unprotect(1);
        return indices;
    } else Rf_error("Provided pointer is not a liu_index object");
}

SEXP r_search_by_range(SEXP index_ptr, SEXP start, SEXP end) {
    // Checking for index class and calling right function int or double
    if (TYPEOF(index_ptr) != EXTPTRSXP) {
        Rf_error("Argument must be an External Pointer (liu_index)");
    }

    if (R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_int")) {
        // Getting root
        int_node* root = (int_node*)R_ExternalPtrAddr(index_ptr);
        
        if (root == NULL) {
            Rf_error("Tree pointer is NULL (empty or corrupted)");
        }

        // Converting key to int
        int start1 = Rf_asInteger(start);
        int end1 = Rf_asInteger(end);

        int_table table = find_indices_interval_int(root, start1, end1);

        SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

        memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));
        free(table.pointer);
        Rf_unprotect(1);
        return indices;
    } else if (R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_double")){
        // Getting root
        double_node* root = (double_node*)R_ExternalPtrAddr(index_ptr);
        if (root == NULL) {
            Rf_error("Tree pointer is NULL (empty or corrupted)");
        }
        if (!Rf_isReal(start) || !Rf_isReal(end)) {
            Rf_error("Arguments 'start' and 'end' must be of type index(double)");
        }
        // Converting key to double
        double start1 = Rf_asReal(start);
        double end1 = Rf_asReal(end);

        int_table table = find_indices_interval_double(root, start1, end1);

        SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

        memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));
        free(table.pointer);
        Rf_unprotect(1);
        return indices;
    } else {
       Rf_error("Provided pointer is not a liu_index object");
    }
}

SEXP r_search_min(SEXP index_ptr) {
    if (TYPEOF(index_ptr) != EXTPTRSXP) {
        Rf_error("Argument must be an External Pointer (liu_index)");
    }

    if (R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_int")) {
        // Getting root
        int_node* root = (int_node*)R_ExternalPtrAddr(index_ptr);
        
        if (root == NULL) {
            Rf_error("Tree pointer is NULL (empty or corrupted)");
        }

        int_table table = find_indices_min_int(root);
        SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

        memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));

        free(table.pointer);
        Rf_unprotect(1);
        return indices;
    } else if(R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_double")) {
        // Getting root
        double_node* root = (double_node*)R_ExternalPtrAddr(index_ptr);
        
        if (root == NULL) {
            Rf_error("Tree pointer is NULL (empty or corrupted)");
        }

        int_table table = find_indices_min_double(root);
        SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

        memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));

        free(table.pointer);
        Rf_unprotect(1);
        return indices;
    } else {
    Rf_error("Provided pointer is not a liu_index object");
    }
}

SEXP r_search_max(SEXP index_ptr) {
    if (TYPEOF(index_ptr) != EXTPTRSXP) {
        Rf_error("Argument must be an External Pointer (liu_index)");
    }
    if (R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_int")) {
        // Getting root
        int_node* root = (int_node*)R_ExternalPtrAddr(index_ptr);
        
        if (root == NULL) {
            Rf_error("Tree pointer is NULL (empty or corrupted)");
        }

        int_table table = find_indices_max_int(root);
        SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

        memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));

        free(table.pointer);
        Rf_unprotect(1);
        return indices;
    } else if (R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_double")) {
        // Getting root
        double_node* root = (double_node*)R_ExternalPtrAddr(index_ptr);
        
        if (root == NULL) {
            Rf_error("Tree pointer is NULL (empty or corrupted)");
        }

        int_table table = find_indices_max_double(root);
        SEXP indices = Rf_protect(Rf_allocVector(INTSXP, table.size));

        memcpy(INTEGER(indices), table.pointer, table.size * sizeof(int));

        free(table.pointer);
        Rf_unprotect(1);
        return indices;
    } else {
        Rf_error("Provided pointer is not a BTree_Class object");
    }
}

SEXP r_inner_join(SEXP id_vector, SEXP index_ptr, SEXP left){
    // This function is sending arguments to inner_join int or double
    // but it takes argument if it is supposed to be left join

    if (TYPEOF(index_ptr) != EXTPTRSXP) {
        Rf_error("Argument must be an External Pointer (liu_index)");
    }
    if (R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_int")) {
        // Saving as C objects
        int_node* root = (int_node*)R_ExternalPtrAddr(index_ptr);
        int* keys = INTEGER(id_vector);
        int nr_keys = Rf_length(id_vector);
        
        // Building table
        int_table v;
        v.size = (size_t)nr_keys;
        v.pointer = keys;
        dual_int_table table = inner_join_int(v, root, Rf_asBool(left));        

        // Alloc for new vectors
        SEXP out_left = Rf_protect(Rf_allocVector(INTSXP, table.size));
        SEXP out_right = Rf_protect(Rf_allocVector(INTSXP, table.size));
        
        // Copying data
        memcpy(INTEGER(out_left), table.left_indices, table.size * sizeof(int));
        memcpy(INTEGER(out_right), table.right_indices, table.size * sizeof(int));

        // Freing data after inner join
        free(table.left_indices);
        free(table.right_indices);

        // Putting it in list
        SEXP res_list = Rf_protect(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(res_list, 0, out_left);
        SET_VECTOR_ELT(res_list, 1, out_right);

        // Adding atributes used later in R interface
        SEXP names = Rf_protect(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(names, 0, Rf_mkChar("left"));
        SET_STRING_ELT(names, 1, Rf_mkChar("right"));
        Rf_setAttrib(res_list, R_NamesSymbol, names);
        Rf_unprotect(4);
        return res_list;
    } else if (R_ExternalPtrTag(index_ptr) == Rf_install("liu_index_double")) {
        // Saving as C objects
        double_node* root = (double_node*)R_ExternalPtrAddr(index_ptr);
        double* keys = REAL(id_vector);
        int nr_keys = Rf_length(id_vector);

        // Building table
        double_table v;
        v.size = (size_t)nr_keys;
        v.pointer = keys;
        dual_int_table table = inner_join_double(v, root, Rf_asBool(left));        
 
        // Alloc for new vectors
        SEXP out_left = Rf_protect(Rf_allocVector(INTSXP, table.size));
        SEXP out_right = Rf_protect(Rf_allocVector(INTSXP, table.size));
        
        // Copying data
        memcpy(INTEGER(out_left), table.left_indices, table.size * sizeof(int));
        memcpy(INTEGER(out_right), table.right_indices, table.size * sizeof(int));

        // Freing data after inner join
        free(table.left_indices);
        free(table.right_indices);

        // Putting it in list
        SEXP res_list = Rf_protect(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(res_list, 0, out_left);
        SET_VECTOR_ELT(res_list, 1, out_right);

        // Adding atributes used later in R interface
        SEXP names = Rf_protect(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(names, 0, Rf_mkChar("left"));
        SET_STRING_ELT(names, 1, Rf_mkChar("right"));
        Rf_setAttrib(res_list, R_NamesSymbol, names);
        Rf_unprotect(4);
        return res_list;
    } else {
    Rf_error("Provided pointer is not a BTree_Class object");
    }
}   

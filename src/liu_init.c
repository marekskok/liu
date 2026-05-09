// generated with: tools::package_native_routine_registration_skeleton(".")
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP r_build_tree_from_df(SEXP, SEXP);
extern SEXP r_index_free(SEXP);
extern SEXP r_inner_join(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP r_search_by_key(SEXP, SEXP);
extern SEXP r_search_by_range(SEXP, SEXP, SEXP);
extern SEXP r_search_max(SEXP);
extern SEXP r_search_min(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"r_build_tree_from_df", (DL_FUNC) &r_build_tree_from_df, 2},
    {"r_index_free",         (DL_FUNC) &r_index_free,         1},
    {"r_inner_join",         (DL_FUNC) &r_inner_join,         5},
    {"r_search_by_key",      (DL_FUNC) &r_search_by_key,      2},
    {"r_search_by_range",    (DL_FUNC) &r_search_by_range,    3},
    {"r_search_max",         (DL_FUNC) &r_search_max,         1},
    {"r_search_min",         (DL_FUNC) &r_search_min,         1},
    {NULL, NULL, 0}
};

void R_init_liu(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

#' @useDynLib liulibrary
"_PACKAGE"
#'
#' @title
#' Creating index
#'
#' @description
#' I've no time for this.
#'
#' @param df data frame
#' @param column_name dont'make me explain
#'
#' @return
#' Returns c tree object
#'
#' @export
create_btree_from_df <- function(df, column_name) {
  if (!is.data.frame(df)) stop("Argument 'data' musi być ramką danych.")
  
  ptr <- .Call("r_build_tree_from_df", df, as.character(column_name), PACKAGE = "liulibrary")
  return(ptr)
#  structure(list(ptr = ptr), class = "btree")
}
#'
#' @title
#' Searching for key
#'
#' @description
#' I've no time for this.
#'
#' @param tree tree
#' @param key key
#'
#' @return
#' Returns index number
#'
#' @export
btree_search <- function(tree, key) {
  res <- .Call("r_search_by_key", tree, as.integer(key), PACKAGE = "liulibrary")
  return(res)
}
#' @title
#' Free free palestine
#'
#' @description
#' I've no time for this.
#'
#' @param tree tree
#' @param key key
#'
#' @return
#' Returns index number
#'
#' @export
btree_free <- function(tree) {
  .Call("btree_free", tree, PACKAGE = "liulibrary")
  invisible(NULL)
}
#'
#' @title
#' Searching for key by interval
#'
#' @description
#' I've no time for this.
#'
#' @param tree tree
#' @param key key
#'
#' @return
#' Returns index number
#'
#' @export
btree_interval_search <- function(tree, start, end) {
  res <- .Call("r_search_by_interval", tree, as.integer(start), as.integer(end), PACKAGE = "liulibrary")
  return(res)
}
#'
#' @title
#' Searching minimum
#'
#' @description
#' I've no time for this.
#'
#' @param tree tree
#' @param key key
#'
#' @return
#' Returns index number
#'
#' @export
btree_search_min <- function(tree) {
  res <- .Call("r_search_min", tree, PACKAGE = "liulibrary")
  return(res)
}
#'
#' @title
#' Searching minimum
#'
#' @description
#' I've no time for this.
#'
#' @param tree tree
#' @param key key
#'
#' @return
#' Returns index number
#'
#' @export
btree_search_max <- function(tree) {
  res <- .Call("r_search_max", tree, PACKAGE = "liulibrary")
  return(res)
}

















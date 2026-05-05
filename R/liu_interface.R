#' @useDynLib liu
"_PACKAGE"
#'
#' @title
#' Build LIU Index
#'
#' @description
#' This function if foundation of LIU package. It builds a high-performance 
#' SQL-like B+Tree index for a data frame column, enabling fast joins and lookups.
#' Works only for columns of ints or doubles.
#'
#' @param df A data frame to be indexed.
#' @param column_name A character string specifying the column to index. 
#' (int or double column)
#'
#' @return
#' C LIU index object
#'
#' @examples
#' idx <- liu_build(mtcars, "mpg")
#' 
#' @export
liu_build <- function(df, column_name) {
  if (!is.data.frame(df)) {
    stop("Input df must be data frame type")
  }
  if (!(column_name %in% names(df))) {
    stop("Column not found in data frame")
  }
  col_data <- df[[column_name]]
  if (!is.numeric(col_data)) {
    stop("Column must be numeric (integer or double)")
  }
  
  ptr <- .Call("r_build_tree_from_df", df, as.character(column_name), PACKAGE = "liu")
  return(ptr)
}
#'
#' @title
#' Search for Key in LIU Index
#'
#' @description
#' Performs a fast lookup in the LIU index to find all row indices associated 
#' with a specific key.
#'
#' @param index LIU index object (external pointer)
#' @param key scalar key (int or double must match LIU index type) to search for
#' @return
#'
#' An integer vector of row indices where the key was found. 
#' Returns an empty vector if the key does not exist.
#'
#' @examples
#' row_ids <- liu_search(idx, 110)
#' mtcars[row_ids, ]
#' 
#' @export
liu_search <- function(index, key) {
  if (is.null(index) || typeof(index) != "externalptr" || !inherits(index, "liu_index")) {
    stop("Index must be a valid LIU external pointer.")
  }
  res <- .Call("r_search_by_key", index, key, PACKAGE = "liu")
  return(res)
}
#'
#' @title
#' Free LIU Index Memory
#'
#' @description
#' Explicitly releases the memory allocated for the B+Tree index in C. 
#' Use this when index is no longer needed to prevent memory leaks.
#'
#' @param index LIU index object (external pointer)
#'
#' @examples
#' idx <- liu_build(mtcars, "hp")
#' # ...
#' liu_free(idx)
#'
#' @export
liu_free <- function(index) {
  if (is.null(index) || typeof(index) != "externalptr" || !inherits(index, "liu_index")) {
    stop("Index must be a valid LIU external pointer.")
  }
  .Call("r_index_free", index, PACKAGE = "liu")
  invisible(NULL)
}
#'
#' @title
#' Range Search in LIU Index
#'
#' @description
#' Finds all row indices within a specified numerical range [start, end) in LIU. 
#' index. This operation is very efficient due to the B+Tree structure.
#'
#' @param index A LIU index object (external pointer).
#' @param start Numeric, beginning of the range (inclusive).
#' @param end Numeric, end of the range (exclusive).
#'
#' @return
#' Integer vector of row indices for keys within the range.
#' 
#' @examples
#' Find rows where 10 <= key < 50
#' rows <- liu_search_range(idx, 10, 50)
#'
#' @export
liu_search_range <- function(index, start, end) {
  if (is.null(index) || typeof(index) != "externalptr" || !inherits(index, "liu_index")) {
    stop("Index must be a valid LIU external pointer.")
  }
  if (!is.numeric(start) || !is.numeric(end)) {
    stop("Both 'start' and 'end' must be numeric values.")
  }
  if (start >= end) {
    return(integer(0))
  }
  
  res <- .Call("r_search_by_range", index, as.numeric(start), as.numeric(end), PACKAGE = "liu")
  res <- sort(res)
  return(res)
}
#'
#' @title
#' Search for Index of Minimum Key in LIU Index
#'
#' @description
#' Search for the smallest keys in LIU index and returs their row indices
#'
#' @param index A LIU index object (external pointer).
#'
#' @return
#' An integer vector of row indices corresponding to the minimum key.
#'
#' @examples
#' # Get row indices for the smallest value in the index
#' # min_rows <- liu_search_min(idx)
#'
#' @export
liu_min <- function(index) {
  if (is.null(index) || typeof(index) != "externalptr" || !inherits(index, "liu_index")) {
    stop("Index must be a valid LIU external pointer.")
  }
  
  res <- .Call("r_search_min", index, PACKAGE = "liu")
  return(res)
}
#'
#' @title
#' Search for Index of Maximum Key in LIU Index
#'
#' @description
#' Search for the largest keys in LIU index and returs their row indices
#'
#' @param index A LIU index object (external pointer).
#'
#' @return
#' An integer vector of row indices corresponding to the maximum key.
#'
#' @examples
#' # Get row indices for the smallest value in the index
#' # max_rows <- liu_search_max(idx)
#'
#' @export
liu_max <- function(index) {
  if (is.null(index) || typeof(index) != "externalptr" || !inherits(index, "liu_index")) {
    stop("Index must be a valid LIU external pointer.")
  }
  
  res <- .Call("r_search_max", index, PACKAGE = "liu")
  return(res)
}
#'
#' @title
#' Fast Inner Join
#'
#' @description
#' Performs a high-performance Join between two data frames using a LIU index. 
#' For now only inner (default) and left join are available.
#' 
#' @param df_left Data frame (left side of the join).
#' @param column_name Name of the join numeric column (must exist in both data frames).
#' @param df_right The "indexed" data frame (right side of the join).
#' @param index A LIU index object built on the join column of `df_right`.
#' @param how A character string "inner" or "left".
#'
#' @return
#' Returns merged data frame.
#'
#' @examples
#' idx <- liu_build(df_b, "id")
#' merged <- liu_join(df_a, "id", df_b, idx)
#'
#' idx <- liu_build(df_b, "id")
#' merged <- liu_join(df_a, "id", df_b, idx, "left")
#' 
#' @export
liu_join <- function(df_left, column_name, df_right, index, how="inner") {
  if (!is.character(column_name)) {
    stop("Column_name must be character")
  }
  if (!is.data.frame(df_right) || !is.data.frame(df_left)) {
    stop("Input df must be data frame type")
  }
  if (!(column_name %in% names(df_left)) || !(column_name %in% names(df_right))) {
    stop("Column not in column names")
  }
  if (is.null(index) || typeof(index) != "externalptr" || !inherits(index, "liu_index")) {
    stop("Index must be a valid LIU external pointer.")
  }
  
  id_vector <- df_left[[column_name]]
  
  if (how=="inner"){
  indices <- .Call("r_inner_join", id_vector, index, FALSE, PACKAGE = "liu")
  } else if (how == "left") {
  indices <- .Call("r_inner_join", id_vector, index, TRUE, PACKAGE = "liu")
  } else {
    stop("how= can be inner or left")
  }
  res_left <- lapply(df_left, function(x) x[indices$left])
    
  right_cols <- setdiff(names(df_right), column_name)
  res_right <- lapply(df_right[right_cols], function(x) x[indices$right])
    
  df_final <- c(res_left, res_right)
  
  class(df_final) <- "data.frame"
  attr(df_final, "row.names") <- .set_row_names(length(indices$left))

  return(df_final)
}














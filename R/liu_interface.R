#' @useDynLib liu
"_PACKAGE"
#'
#' @title
#' Build LIU Index
#'
#' @description
#' This function if foundation of LIU package. It builds a high-performance 
#' SQL-like B+Tree index for a data frame column, enabling fast joins and lookups.
#' Works only for columns of ints or doubles. Values of given column will be 
#' put in B+Tree with their row numbers. It ignores NA in given column.
#'
#' @param df Data frame to be indexed.
#' @param column_name Character string specifying the column to index. 
#' (int or double column)
#'
#' @return
#' C LIU index object (pointer).
#'
#' @examples
#' \dontrun{
#' idx <- liu_build(mtcars, "mpg")
#' }
#' @export
liu_build <- function(df, column_name) {
  if (!is.data.frame(df)) {
    stop("Input df must be of a data frame type")
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
#' Search for Keys in LIU Index
#'
#' @description
#' Performs a fast lookup in the LIU index to find all row indices associated 
#' with given vector of keys. It ignores NA in keys vector.
#'
#' @param index LIU index object (external pointer).
#' @param key Vector of keys (int or double must match LIU index type) to search for.
#' @return
#' Integer vector of row indices where the keys were found. 
#' Returns an empty vector if none of the keys exist.
#'
#' @examples
#' \dontrun{
#' row_idx <- liu_search(idx, 110)
#' df[row_ids, ]
#' 
#' row_ids <- liu_search(idx, c(6.7,21.15))
#' df[row_idx, ]
#' }
#' @export
liu_search <- function(index, key) {
  key <- as.vector(na.omit(key))
  if (is.null(index) || typeof(index) != "externalptr") {
    stop("Index must be a valid LIU external pointer.")
  }
  if (!inherits(index, "liu_pointer_int") && !inherits(index, "liu_pointer_double")){
    stop("External pointer is not liu_pointer")
  }
  if (inherits(index, "liu_pointer_int") && !is.integer(key)){
    stop("LIU pointer is type int, but given key isn't, R treats normal number as doubles")
  }
  if (inherits(index, "liu_pointer_double") && !is.double(key)){
    stop("LIU pointer is type double, but given key isn't")
  }
  res <- .Call("r_search_by_key", index, key, PACKAGE = "liu")
  return(res)
}
#'
#' @title
#' Free LIU Index Memory
#'
#' @description
#' Explicitly releases the memory allocated for the LIU index in C. 
#' Use this when index is no longer needed to prevent memory leaks.
#'
#' @param index LIU index object (external pointer).
#'
#' @examples
#' \dontrun{
#' idx <- liu_build(mtcars, "hp")
#' # ...
#' liu_free(idx)
#'}
#' @export
liu_free <- function(index) {
  if (is.null(index) || typeof(index) != "externalptr") {
    stop("Index must be a valid LIU external pointer.")
  }
  if (inherits(index, "liu_pointer_int") || inherits(index, "liu_pointer_double")){
  .Call("r_index_free", index, PACKAGE = "liu")
    attributes(index) <- NULL
    index <- NULL
    invisible(NULL)
  }
}
#'
#' @title
#' Range Search in LIU Index
#'
#' @description
#' Finds all row indices with keys within a specified numerical range [start, end) in LIU. 
#' index. This operation is very efficient due to the B+Tree structure. You can leave
#' the start, the end, or both blank for an unbounded search.
#'
#' @param index A LIU index object (external pointer).
#' @param start Numeric scalar, beginning of the range (inclusive). Must
#' match index type (int or double).
#' @param end Numeric scalar, end of the range (exclusive). Must
#' match index type (int or double).
#'
#' @return
#' Integer vector of row indices with keys within the range.
#' 
#' @examples
#' \dontrun{
#' Find rows where 10 <= key < 50
#' rows <- liu_search_range(idx, 10, 50)
#' df[rows]
#'
#' Row indices with keys greater or equal to 2.5
#' rows <- liu_search_range(idx, 2.5)
#'}
#' @export
liu_search_range <- function(index, start=NA, end=NA) {
  if (is.null(index) || typeof(index) != "externalptr") {
    stop("Index must be a valid LIU external pointer.")
  }
  if (!inherits(index, "liu_pointer_int") && !inherits(index, "liu_pointer_double")){
    stop("External pointer is not liu_pointer")
  }
  if (inherits(index, "liu_pointer_double") && is.na(start)){
    start <- -Inf
  }
  if (inherits(index, "liu_pointer_double") && is.na(end)){
    end <- Inf
  }
  if (inherits(index, "liu_pointer_int") && is.na(start)){
    list <- .Call("r_search_min", index, PACKAGE = "liu")
    start <- as.integer(list[[2]])
  }
  if (inherits(index, "liu_pointer_int") && is.na(end)){
    list <- .Call("r_search_max", index, PACKAGE = "liu")
    end <- as.integer(list[[2]] + 1)
  }
  
  if (inherits(index, "liu_pointer_int") && (!is.integer(start) || !is.integer(end))){
    stop("LIU pointer is type int, but given start or end isn't, R treats normal number as doubles")
  }
  if (inherits(index, "liu_pointer_double") && (!is.double(start) || !is.double(end))){
    stop("LIU pointer is type double, but given start or end isn't")
  }
  if (start >= end) {
    return(integer(0))
  }
  
  res <- .Call("r_search_by_range", index, start, end, PACKAGE = "liu")
  res <- sort(res)
  return(res)
}
#'
#' @title
#' Minimum Key in LIU Index
#'
#' @description
#' Search for the smallest key in LIU index and returs their row indices.
#'
#' @param index A LIU index object (external pointer).
#'
#' @return
#' An integer vector of row indices corresponding to the minimum key.
#'
#' @examples
#' \dontrun{
#' Get row indices for the smallest key in the index
#' min_rows <- liu_min(idx)
#'}
#' @export
liu_min <- function(index) {
  if (is.null(index) || typeof(index) != "externalptr") {
    stop("Index must be a valid LIU external pointer.")
  }
  if (!inherits(index, "liu_pointer_int") && !inherits(index, "liu_pointer_double")){
    stop("External pointer is not liu_pointer")
  }
  
  res <- .Call("r_search_min", index, PACKAGE = "liu")
  return(res[[1]])
}
#'
#' @title
#' Maximum Key in LIU Index
#'
#' @description
#' Search for the largest key in LIU index and returns their row indices.
#'
#' @param index A LIU index object (external pointer).
#'
#' @return
#' An integer vector of row indices corresponding to the maximum key.
#'
#' @examples
#' \dontrun{
#' # Get row indices for the smallest key in the index
#' max_rows <- liu_max(idx)
#' }
#' @export
liu_max <- function(index) {
  if (is.null(index) || typeof(index) != "externalptr") {
    stop("Index must be a valid LIU external pointer.")
  }
  if (!inherits(index, "liu_pointer_int") && !inherits(index, "liu_pointer_double")){
    stop("External pointer is not liu_pointer")
  }
  
  res <- .Call("r_search_max", index, PACKAGE = "liu")
  return(res[[1]])
}
#'
#' @title
#' Fast Inner Join
#'
#' @description
#' Performs a high-performance Join between two data frames using a LIU index. 
#' For now only inner (default) and left join are available. It takes 4
#' arguments: left data table, name of one of it's columns, right data
#' table, index built on it. Type of chosen column must match index type.
#' Indexes doesn't take NA, so NA in given column are ignored. It returns the same
#' data frame as merge(df_left, df_right, "id", incomparables = NA)
#' 
#' @param df_left Data frame (left side of the join).
#' @param column_name Name of the join numeric column (must exist in left data frames).
#' @param df_right The "indexed" data frame (right side of the join).
#' @param index A LIU index object built on the join column of `df_right`.
#' @param how A character string "inner" or "left".
#'
#' @return
#' Returns merged data frame.
#'
#' @examples
#' \dontrun{
#' idx <- liu_build(df_b, "id")
#' merged <- liu_join(df_a, "id", df_b, idx)
#'
#' idx <- liu_build(df_b, "id")
#' merged <- liu_join(df_a, "id", df_b, idx, "left")
#' ?
#' @export
liu_join <- function(df_left, column_name, df_right, index, how="inner") {
  if (!is.character(column_name)) {
    stop("Column_name must be character")
  }
  if (!is.data.frame(df_right) || !is.data.frame(df_left)) {
    stop("Input df must be data frame type")
  }
  if (!(column_name %in% names(df_left))) {
    stop("Column not in column names")
  }
  if (is.null(index) || typeof(index) != "externalptr") {
    stop("Index must be a valid LIU external pointer.")
  }
  if (!inherits(index, "liu_pointer_int") && !inherits(index, "liu_pointer_double")){
    stop("External pointer is not liu_pointer")
  }
  if (how == "inner") {
    left = FALSE
  } else if (how == "left"){
    left = TRUE
  } else {
    stop("Only inner and left are available")
  }
  id_vector <- df_left[[column_name]]
  
  if (inherits(index, "liu_pointer_int") && !is.integer(id_vector)){
    stop("Column type and index type don't match")
  }
  if (inherits(index, "liu_pointer_double") && !is.double(id_vector)){
    stop("Column type and index type don't match")
  }
  merged <- .Call("r_inner_join", df_left, column_name, df_right, index, left)
  return (merged)
}
#'
#' @title
#' Is In LIU Index
#'
#' @description
#' Checks if given keys are present in LIU index. For NA it returns FALSE.
#'
#' @param index A LIU index object (external pointer).
#' @param keys Vector of keys (int or double must match LIU index type) to check.
#' @return
#'
#' @return
#' Vector of logical values.
#' 
#' @examples
#' \dontrun{
#' Checks in int index if 2, NA, 5 are present
#' logical <- liu_isin(idx, as.integer(c(2,NA,5)))
#' 
#' Checks in double index if 6.7 is present
#' logical <- liu_isin(idx, 6.7)
#' }
#' @export
liu_isin <- function(index, keys){
  if (length(keys) == 0){
    return (as.logical(c()))
  }
  if (is.null(index) || typeof(index) != "externalptr") {
    stop("Index must be a valid LIU external pointer.")
  }
  if (!inherits(index, "liu_pointer_int") && !inherits(index, "liu_pointer_double")){
    stop("External pointer is not liu_pointer")
  }
  if (inherits(index, "liu_pointer_int") && !is.integer(keys)){
    stop("LIU pointer is type int, but given key isn't, R treats normal number as doubles")
  }
  if (inherits(index, "liu_pointer_double") && !is.double(keys)){
    stop("LIU pointer is type double, but given key isn't")
  }
  res <- logical(length(keys))
  
  for (i in 1:length(keys)){
    if (length(liu_search(index, keys[i])) != 0){
      res[i] <- TRUE
    }
  }
  return (res)
}
#'
#' @title
#' Count in Range
#'
#' @description
#' Count all row indices with keys within a specified numerical range [start, end) in LIU. 
#' index. This operation is very efficient due to the B+Tree structure. You can leave
#' the start, the end, or both blank for an unbounded count.
#'
#' @param index A LIU index object (external pointer).
#' @param start Numeric scalar, beginning of the range (inclusive). Must
#' match index type (int or double).
#' @param end Numeric scalar, end of the range (exclusive). Must
#' match index type (int or double).
#'
#' @return
#' Integer number of row indices with keys within the range.
#' 
#' @examples
#' \dontrun{
#' Count rows where 10 <= key < 50
#' rows <- liu_count(idx, 10, 50)
#'
#' Count row indices with keys greater or equal to 2.5
#' rows <- liu_count(idx, 2.5)
#' }
#' @export
liu_count <- function(index, start=NA, end=NA) {
  return(sum(as.logical(liu_search_range(index, start, end))))
}










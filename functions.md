### Functions:
**liu_build()** - This function is foundation of LIU package. It builds index with keys from given column, but ignores NA. For now works only with columns of ints or doubles.
```R
idx <- liu_build(df, "id")
```
It returns external pointer to the c object (B+tree). You can check it's type by calling.
```R
> idx
<pointer: 0x559b1e9c5b10>
attr(,"class")
[1] "liu_pointer_int"
attr(,"column_name")
[1] "id"
# or
> idx
<pointer: 0x559b1e9c5b10>
attr(,"class")
[1] "liu_pointer_double"
attr(,"column_name")
[1] "id"
```
**liu_free()** - Explicitly releases the memory allocated for the LIU index in C. Use this when index is no longer needed to prevent memory leaks.
```R
liu_free(idx)
```
**liu_search()** - Performs a fast lookup in the LIU index to find all row indices associated with given vector of keys. It ignores NA in keys vector. Vector of keys (int or double must match LIU index type) to search for.
```R
row_idx <- liu_search(idx, 110)
df[row_ids, ]

row_ids <- liu_search(idx, c(6.7,21.15))
df[row_idx, ]
```
**liu_search_range()** - Finds all row indices with keys within a specified numerical range [start, end) in LIU. index. This operation is very efficient due to the B+Tree structure. You can leave the start, the end, or both blank for an unbounded count.
```R
# Find rows where 10 <= key < 50
rows <- liu_search_range(idx, 10, 50)
df[rows]

# Row indices with keys greater or equal to 2.5
rows <- liu_search_range(idx, 2.5)
```
**liu_min()** - Search for the smallest key in LIU index and returs their row indices.
```R
# Get row indices for the smallest key in the index
min_rows <- liu_search_min(idx)
```
**liu_max()** - Search for the largest key in LIU index and returs their row indices.
```R
# Get row indices for the largest key in the index
max_rows <- liu_search_min(idx)
```
**liu_join()** - Performs a high-performance Join between two data frames using a LIU index. For now only inner (default) and left join are available. It takes 4 arguments: left data table, name of one of it's columns, right data table, index built on it. Type of chosen column must match index type. Indexes doesn't take NA, so NA in given column are ignored. It returns the same data frame as merge(df_left, df_right, "id", incomparables = NA)
```R
# inner
idx <- liu_build(df_b, "id")
merged <- liu_join(df_a, "id", df_b, idx)
# left
idx <- liu_build(df_b, "id")
merged <- liu_join(df_a, "id", df_b, idx, "left")
```
**liu_isin()** - Checks if given keys are present in LIU index. For NA it returns FALSE. Returns vector of logical values.
```R
# Checks in int index if 2, NA, 5 are present
logical <- liu_isin(idx, as.integer(c(2,NA,5)))

# Checks in double index if 6.7 is present
logical <- liu_isin(idx, 6.7)
```
**liu_count()** - Count all row indices with keys within a specified numerical range [start, end) in LIU. index. You can leave the start, the end, or both blank for an unbounded search.
```R
# Count rows where 10 <= key < 50
rows <- liu_count(idx, 10, 50)

# Count row indices with keys greater or equal to 2.5
rows <- liu_count(idx, 2.5)
```
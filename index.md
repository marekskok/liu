<img src = "man/figures/liu_logo_2.jpg" alt="LIU Logo" width = "200">

## LIU - Lightweight Index Units

This library implements SQL-like indexes for data frames in R. It allows to index columns of data frames by creating light B+trees with pairs key-row_number. Thanks to that searching is O(log n), which makes few other functions (exp. min, max, count, merge) much faster.

##### Contents:
1. [Installation](installation.md)
1. [Functions](functions.md)
2. [Performance](performance.md)












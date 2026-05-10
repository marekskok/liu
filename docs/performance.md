---
layout: default
title: Performance
nav_order: 4
parent: Home
---
### Performance
Node of B+tree in which data is stored contains 16-32 keys, so in the pesymistic case 1'000'000 keys would need 66'669 nodes. In that case height of tree is 5. This means that to find any value among a million rows, the algorithm only needs to perform a handful of memory jumps. You don't have to imagine the speed, see the benchmarks below.
#### liu_search()
Test speed of liu_search in huge data frame:
```R
library(liu)
library(microbenchmark)

df <- data.frame(val = sample(1:100000, 1000000, replace = TRUE))
idx <- liu_build(df, "val")
target <- df$val[500000]

res_search <- microbenchmark(
  base_r = which(df$val == target),
  liu    = liu_search(idx, target),
  times = 100,
  unit = "micro"
)
print(res_search)
#Unit: microseconds
#   expr      min        lq       mean    median       uq      max neval
# base_r 1023.691 1056.4990 1480.38757 1081.7730 1124.501 9137.810   100
#    liu    2.698    3.5085   12.38893    5.0245   19.567   50.478   100
```
#### liu_join()
These are the results that really weren't obvious. First version of ```liu_join()``` for huge data frames was only about 4-5 times faster than base R merge. Thanks to implementation of multi-threading and moving last part, which is assembling result data frame from R to C, performance drastically improved.
Now it outclasses base R and win with dplyr 4 or 5 times. (Obviously it depends on the size of data frames, but I give some numbert to make it more intersting)
```R
library(liu)
library(microbenchmark)
library(dplyr)

df_a <- data.frame(id = sample(1:100000, 500000, replace = TRUE), val = runif(500000))
df_b <- data.frame(id = 1:500000, info = runif(500000))
idx_b <- liu_build(df_b, "id")

res_join <- microbenchmark(
  base_merge = merge(df_a, df_b, "id", incomparables = NA),
  dplyr_join = df_a %>% inner_join(df_b, by = "id", na_matches = "never"),
  liu_join   = liu_join(df_a, "id", df_b, idx_b),
  times = 10
)
print(res_join)
# Unit: milliseconds
#        expr        min         lq       mean     median         uq       max neval
#  base_merge 443.120667 492.273238 536.813204 513.595328 559.451582 699.27417    10
#  dplyr_join  32.736661  33.268801  56.592572  47.723909  71.180185 128.90706    10
#    liu_join   8.110335   8.463431   9.346169   9.422417   9.765384  11.18143    10
```
#### liu_min()
Speed of this function lies in the srtucture of B+tree. LIU simply follows the tree path to the leftmost or rightmost leaf ($O(\log n)$), instead of scanning whole column.
```R
library(liu)
library(microbenchmark)

df <- data.frame(val = runif(1000000))
idx <- liu_build(df, "val")

res_min <- microbenchmark(
  base_r = which(df$val == min(df$val, na.rm = TRUE)),
  liu    = liu_min(idx),
  times = 10,
  unit = "micro"
)
print(res_min)
#Unit: microseconds
#   expr      min       lq      mean    median       uq      max neval
# base_r 2352.779 2366.384 2469.1265 2422.8550 2448.426 2917.070    10
#    liu    0.785    1.228    6.6619    8.4655    9.100   16.443    10
```




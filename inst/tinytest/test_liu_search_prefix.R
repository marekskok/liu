library(liu)
library(tinytest)

df <- data.frame(
  id = as.integer(c(1,2,3,4,5)),
  word = c("Poland", NA, "England", "Portugal", "Pakistan")
)

idx <- liu_build(df, "word")
expect_error(liu_search_prefix(idx, as.integer(2)))
expect_error(liu_search_prefix(idx))
expect_identical(liu_search_prefix(idx, "Po"), as.integer(c(1,4)))
expect_identical(liu_search_prefix(idx, "P"), as.integer(c(1,4,5)))
expect_identical(liu_search_prefix(idx, as.character(NA)), as.integer(c()))
expect_identical(liu_search_prefix(idx, as.character("E")), as.integer(c(3)))
expect_identical(liu_search(idx, "Z"), as.integer(c()))
liu_free(idx)
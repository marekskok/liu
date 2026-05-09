library(liu)
library(tinytest)

df <- data.frame(
  id = as.integer(c(1,1,NA,3,5)),
  val = c(6.7,6.7,2.3,NA,4.1)
)
idx1 <- liu_build(df, "id")
expect_error(liu_count(df))
expect_identical(liu_count(idx1), as.integer(4))
expect_identical(liu_count(idx1, as.integer(1), as.integer(2)), as.integer(2))
expect_identical(liu_count(idx1, end=as.integer(4)), as.integer(3))
liu_free(idx1)

library(liu)
library(tinytest)

df <- data.frame(
  id = as.integer(c(1,2,NA,5,7,5,2)),
  val = c(1.6,1.6,0,NA,6.7,8.9,123.3)
)
idx1 <- liu_build(df, "id")
expect_error(liu_isin(idx))
expect_error(liu_isin(df, "id"))
expect_identical(liu_isin(idx1, as.integer(c(2,NA,4,7))), c(TRUE,FALSE,FALSE,TRUE))
expect_identical(liu_isin(idx1, as.integer(c())), as.logical(c()))
expect_error(liu_isin(idx1, 2.5))
expect_identical(liu_isin(idx1, as.integer(5)), as.logical(TRUE))
liu_free(idx1)

idx2 <- liu_build(df, "val")
expect_identical(liu_isin(idx2, c(1.6,NA,3.7)), as.logical(c(TRUE,FALSE,FALSE)))
expect_identical(liu_isin(idx2, as.double(c())), as.logical(c()))
expect_error(liu_isin(idx1, as.integer(5)))
liu_free(idx2)














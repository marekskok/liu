library(liu)
library(tinytest)

expect_error(liu_free(NULL))
expect_error(liu_free(1))

df <- data.frame(
  id1 = as.integer(c(1,2,3)),
  val = c(2.5,6.7,1.3),
  word = c("test1", "test2", "test3")
)
idx <- liu_build(df, "id1")
expect_silent(liu_free(idx))
expect_silent(liu_free(idx))

idx <- liu_build(df, "val")
expect_silent(liu_free(idx))
expect_silent(liu_free(idx))

idx <- liu_build(df, "word")
expect_silent(liu_free(idx))
expect_silent(liu_free(idx))

df <- data.frame(
  id1 = as.integer(NA),
  val = as.double(NA),
  word = as.character(NA)
)
idx1 <- liu_build(df, "id1")
expect_warning(liu_free(idx1))

idx2 <- liu_build(df, "val")
expect_warning(liu_free(idx2))

idx3 <- liu_build(df, "word")
expect_warning(liu_free(idx3))

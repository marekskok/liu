library(liu)
library(tinytest)

# 2 maximum
df <- data.frame(
  id1 = as.integer(c(2,6,3,NA,6)),
  val = c(NA, 6.7, 1.5, 6.7, 2.3)
)
idx1 <- liu_build(df, "id1")
expect_error(liu_max(5))
expect_identical(liu_max(idx1), as.integer(c(2,5)))
liu_free(idx1)

idx2 <- liu_build(df, "val")
expect_identical(liu_max(idx2), as.integer(c(2,4)))
liu_free(idx2)

# 1 maximum
df <- data.frame(
  id1 = as.integer(c(2,6,3,NA)),
  val = c(NA, 1.5, 6.7, 2.3)
)

idx1 <- liu_build(df, "id1")
expect_identical(liu_max(idx1), as.integer(2))
liu_free(idx1)

idx2 <- liu_build(df, "val")
expect_identical(liu_max(idx2), as.integer(3))
liu_free(idx2)

# no data
df <- data.frame(
  id1 = as.integer(NA),
  val = as.double(NA)
)
idx1 <- liu_build(df, "id1")
expect_identical(liu_max(idx1), as.integer(c()))
expect_warning(liu_free(idx1))

idx2 <- liu_build(df, "val")
expect_identical(liu_max(idx2), as.integer(c()))
expect_warning(liu_free(idx2))

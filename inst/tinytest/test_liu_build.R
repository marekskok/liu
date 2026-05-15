library(liu)
library(tinytest)

df <- data.frame(
  id1 = as.integer(c(1,2,3)),
  val = c(2.5,6.7,3.8)
)

# Wrong arguments tests
expect_error(liu_build(df))
expect_error(liu_build(NULL, "Alysa"))
expect_error(liu_build(df, "id2"))

# Checking attributes of pointers
idx1 <- liu_build(df, "id1")
expect_true(inherits(idx1, "liu_pointer_int"))
liu_free(idx1)

idx2 <- liu_build(df, "val")
expect_true(inherits(idx2, "liu_pointer_double"))
liu_free(idx2)

# Empty pointers but still good type
df <- data.frame(
  id1 = as.integer(c(NA,NA)),
  val = as.double(c(NA,NA))
)
idx1 <- liu_build(df, "id1")
expect_true(inherits(idx1, "liu_pointer_int"))
liu_free(idx1)

idx2 <- liu_build(df, "val")
expect_true(inherits(idx2, "liu_pointer_double"))
liu_free(idx2)





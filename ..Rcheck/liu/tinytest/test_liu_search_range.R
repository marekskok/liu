library(liu)
library(tinytest)

df <- data.frame(
  id1 = as.integer(c(3,-2,0,1,1,NA,6,7,1,-2)),
  val = c(3.4,2.6,1.7,6.7,NA,0.2,-2.2,2.6,1.7,1.7)
)
# testing int index
idx1 <- liu_build(df, "id1")
expect_error(liu_search_range(idx1, start="Alysa", end=as.integer(5)))
expect_error(liu_search_range(idx1, start=as.integer(5), end="Alysa"))
expect_error(liu_search_range(df))
expect_error(liu_search_range(idx1, 1, as.integer(8)))
expect_identical(liu_search_range(idx1), as.integer(c(1,2,3,4,5,7,8,9,10)))
expect_identical(liu_search_range(idx1, as.integer(5)),as.integer(c(7,8)))
expect_identical(liu_search_range(idx1, end=as.integer(1)),as.integer(c(2,3,10)))
expect_identical(liu_search_range(idx1, as.integer(1), as.integer(7)),as.integer(c(1,4,5,7,9)))
expect_identical(liu_search_range(idx1, as.integer(8), as.integer(12)),as.integer(c()))
expect_identical(liu_search_range(idx1, as.integer(7), as.integer(1)),as.integer(c()))
liu_free(idx1)

#testing double index
idx2 <- liu_build(df, "val")
expect_error(liu_search_range(idx2, start="Alysa", end=as.integer(5)))
expect_error(liu_search_range(idx2, start=5, end="Alysa"))
expect_error(liu_search_range(df))
expect_error(liu_search_range(idx2, as.integer(1), 8))
expect_identical(liu_search_range(idx2), as.integer(c(1,2,3,4,6,7,8,9,10)))
expect_identical(liu_search_range(idx2, 3),as.integer(c(1,4)))
expect_identical(liu_search_range(idx2, end=2),as.integer(c(3,6,7,9,10)))
expect_identical(liu_search_range(idx2, 1, 2),as.integer(c(3,9,10)))
expect_identical(liu_search_range(idx2, 8, 12),as.integer(c()))
expect_identical(liu_search_range(idx2, 7, 1),as.integer(c()))
liu_free(idx2)

df <- data.frame(
  id1 = as.integer(NA),
  val = as.double(NA)
)
idx1 <- liu_build(df, "id1")
expect_identical(liu_search_range(idx1), as.integer(c()))
expect_warning(liu_free(idx1))

idx2 <- liu_build(df, "val")
expect_identical(liu_search_range(idx2), as.integer(c()))
expect_warning(liu_free(idx2))

# test bigger dt
df <- data.frame(
  id1 = as.integer(1:1000000),
  val = c(1:1000000)/3
)
idx1<-liu_build(df,"id1")
expect_identical(liu_search_range(idx1, as.integer(5), as.integer(8)), as.integer(c(5,6,7)))
liu_free(idx1)

idx2 <- liu_build(df, "val")
expect_identical(liu_search_range(idx2 ,160,161), as.integer(c(480,481,482)))
liu_free(idx2)
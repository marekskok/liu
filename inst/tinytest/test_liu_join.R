library(liu)
library(tinytest)

# normal data frames
df_left <- data.frame(
  id1 = as.integer(c(1,2,2,NA,5,6)),
  val1 = c(5.5,4.2,5.5,2.7,NA,0.5),
  word1 = c("Poland", "England", NA, "Poland", "France", "Geramny")
)
df_right <- data.frame(
  id2 = as.integer(c(1,NA,1,2,6)),
  val2 = c(4.2,5.5,NA,4.2,0.5),
  word2 = c("Denmark", "England", "Poland", "England", "France")
)

idx1 <- liu_build(df_right, "id2")
expect_error(liu_join(df_left,"id2",df_right, idx1))
expect_error(liu_join(df_left,"id1",df_right, NULL))
expect_error(liu_join(df_left, "id1", df_right, idx1, "right"))
# inner:
merged <- liu_join(df_left, "id1", df_right, idx1)
expect_identical(merged$id1, as.integer(c(1,1,2,2,6)))
expect_identical(merged$val2, c(4.2,NA,4.2,4.2,0.5))
# left
merged <- liu_join(df_left, "id1", df_right, idx1, "left")
expect_identical(merged$id1, as.integer(c(1,1,2,2,5,6)))
expect_identical(merged$val2, c(4.2,NA,4.2,4.2,NA,0.5))
liu_free(idx1)

idx2 <- liu_build(df_right, "val2")
expect_error(liu_join(df_left,"id1",df_right, idx2))
# inner
merged <- liu_join(df_left, "val1", df_right, idx2)
expect_identical(merged$id1, as.integer(c(1,2,2,2,6)))
expect_identical(merged$val2, c(5.5,4.2,4.2,5.5,0.5))
# left:
merged <- liu_join(df_left, "val1", df_right, idx2, "left")
expect_identical(merged$id1, as.integer(c(1,2,2,2,NA,6)))
expect_identical(merged$val2, c(5.5,4.2,4.2,5.5,NA,0.5))
liu_free(idx2)

idx3 <- liu_build(df_right, "word2")
expect_error(liu_join(df_left, df_right, "word1", "idx3"))
expect_error(liu_join(df_left, "id1", df_right, idx3))
# inner
merged <- liu_join(df_left, "word1", df_right, idx3)
expect_identical(merged$val1, c(5.5,4.2,4.2,2.7,NA))
expect_identical(merged$id2, as.integer(c(1,NA,2,1,6)))
# left:
merged <- liu_join(df_left, "word1", df_right, idx3, "left")
expect_identical(merged$id2, as.integer(c(1,NA,2,1,6,NA)))
expect_identical(merged$val1, c(5.5,4.2,4.2,2.7,NA,0.5))
liu_free(idx3)

# empty data frames
df_left <- data.frame(
  id1 = as.integer(c(1,2,2,NA,5,6)),
  val1 = c(5.5,4.2,5.5,2.7,NA,0.5),
  word1 = c("Poland", "England", NA, "Poland", "France", "Geramny")
)
df_right <- data.frame(
  id2 = as.integer(NA),
  val2 = as.double(NA),
  word2 = as.character(NA)
)
idx1 <- liu_build(df_right, "id2")
# inner
merged <- liu_join(df_left, "id1", df_right, idx1)
expect_identical(merged$id1, as.integer(c()))
# left
merged <- liu_join(df_left, "id1", df_right, idx1, "left")
expect_identical(merged$id2, as.integer(c(NA,NA,NA,NA,NA)))
expect_warning(liu_free(idx1))

idx2 <- liu_build(df_right, "val2")
# inner
merged <- liu_join(df_left, "val1", df_right, idx2)
expect_identical(merged$id1, as.integer(c()))
# left
merged <- liu_join(df_left, "val1", df_right, idx2, "left")
expect_identical(merged$id2, as.integer(c(NA,NA,NA,NA,NA)))
expect_warning(liu_free(idx2))

idx3 <- liu_build(df_right, "word2")
# inner
merged <- liu_join(df_left, "word1", df_right, idx3)
expect_identical(merged$id1, as.integer(c()))
# left
merged <- liu_join(df_left, "word1", df_right, idx3, "left")
expect_identical(merged$id2, as.integer(c(NA,NA,NA,NA,NA)))
expect_warning(liu_free(idx3))

# change sides
idx1 <- liu_build(df_left, "id1")
# inner
merged <- liu_join(df_right, "id2", df_left, idx1)
expect_identical(merged$id1, as.integer(c()))
# left
merged <- liu_join(df_right, "id2", df_right, idx1, "left")
expect_identical(merged$id2, as.integer(c()))
liu_free(idx1)

idx2 <- liu_build(df_left, "val1")
# inner
merged <- liu_join(df_right, "val2", df_left, idx2)
expect_identical(merged$id1, as.integer(c()))
# left
merged <- liu_join(df_right, "val2", df_left, idx2, "left")
expect_identical(merged$id2, as.integer(c()))
liu_free(idx2)

idx3 <- liu_build(df_left, "word1")
# inner
merged <- liu_join(df_right, "word2", df_left, idx3)
expect_identical(merged$id1, as.integer(c()))
# left
merged <- liu_join(df_right, "word2", df_left, idx3, "left")
expect_identical(merged$id2, as.integer(c()))
liu_free(idx2)

df_left <- data.frame(
  id1 = as.integer(c(1,1,1,1,1)),
  val1 = c(0.5,0.5,0.5,0.5,0.5),
  word1 = c("Poland","Poland","Poland","Poland","Poland")
)

df_right <- data.frame(
  id2 = as.integer(c(1,1,1,1,1)),
  val2 = c(0.5,0.5,0.5,0.5,0.5),
  word2 = c("Poland","Poland","Poland","Poland","Poland")
)
idx1 <- liu_build(df_right, "id2")
expect_equal(length(liu_join(df_left, "id1", df_right, idx1)$id1), 25)
liu_free(idx1)

idx2 <- liu_build(df_right, "val2")
expect_equal(length(liu_join(df_left, "val1", df_right, idx2)$id1), 25)
liu_free(idx2)

idx3 <- liu_build(df_right, "word2")
expect_equal(length(liu_join(df_left, "word1", df_right, idx3)$id1), 25)
liu_free(idx3)


n_unique <- 40000
n_transactions <- 100000
df_right <- data.frame(
  id = as.integer(sample(1:n_unique)),
  val = round(runif(n_unique, 100, 500),2)
)
df_right$word <- sprintf("id_%06d", df_right$id)
df_right <- df_right[sample(nrow(df_right)), ]
df_left <- data.frame(
  id = as.integer(sample(1:(n_unique*2), n_transactions, replace = TRUE)),
  val = round(runif(n_transactions, 100, 500),2)
)
df_left$word <- sprintf("id_%06d", df_left$id)
idx1 <- liu_build(df_right, "id")
res_r <- merge(df_left, df_right, "id")
res_liu <- liu_join(df_left, "id", df_right, idx1)
# test number of rows
expect_true(nrow(res_r) == nrow(res_liu))
liu_free(idx1)

idx2 <- liu_build(df_right, "val")
res_r <- merge(df_left, df_right, by="val")
res_liu <- liu_join(df_left, "val", df_right, idx2)
expect_true(nrow(res_r) == nrow(res_liu))
liu_free(idx2)

idx3 <- liu_build(df_right, "word")
res_r <- merge(df_left, df_right, by="word")
res_liu <- liu_join(df_left, "word", df_right, idx3)
expect_true(nrow(res_r) == nrow(res_liu))
liu_free(idx3)

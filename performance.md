### Performance
In order to show advantages of this library you can run following scripts. Node of B+tree in which data is stored contains 16-32 keys, so in the pesymistic case 1'000'000 keys would need 62'500 nodes. In that case height of tree is 3-4
2. **liu_search()** - Test speed of liu_search in huge data frame:
```R
library(liu)
df <- data.frame(
  id = as.integer(sample(1:200000, 1000000, replace = TRUE))
)
index <- liu_build(df, "id")

start1 <- Sys.time()
for (i in 10000:10500) {
  df[df$id == as.integer(i),]
}
end1 <- Sys.time()

start2 <- Sys.time()
for (i in 10000:10500) {
  df[liu_search(index, as.integer(i)),]
}
end2 <- Sys.time()

cat(end1 - start1) # about: 0.8s
cat(end2 - start2) # about: 0.008s
```







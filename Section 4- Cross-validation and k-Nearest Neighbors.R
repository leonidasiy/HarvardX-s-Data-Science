# 4.1: Nearest Neighbors
library(tidyverse)
library(caret)
library(dslabs)

set.seed(1)
ks <- seq(1, 101, 3)
data(heights)
y <-heights$sex
x <- heights$height

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
f1 <- map_df(ks, function(k) {
   train_set<- heights[-test_index,]
   test_set<- heights[test_index,]
   
   fit<- knn3(sex~height, data=train_set, k=k)
   y_hat<-predict(fit, test_set, type="class")
   F_val <- F_meas(data = y_hat, reference = factor(test_set$sex))
   list(k = k, val = F_val)
})
f1
max(f1$val)
f1$k[which.max(f1$val)]

library(dslabs)
library(caret)
data("tissue_gene_expression")
y <-tissue_gene_expression$y
x <- tissue_gene_expression$x

set.seed(1)
ks <- seq(1, 11, 2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
accuracy <- map_df(ks, function(k) {
  train_set_x = x[-test_index,]
  test_set_x  = x[test_index,]
  train_set_y = y[-test_index]
  test_set_y  = y[test_index]
  
  fit<- knn3(train_set_x, train_set_y, k=k)
  y_hat<-predict(fit, test_set_x, type="class")
  acc <- confusionMatrix(data = y_hat, reference = test_set_y, mode = "everything")$overall["Accuracy"]
  list(k = k, val = acc)
})
accuracy



# 4.2: Cross-validation
library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

table(indexes[1])[3]
table(indexes[1])[4]
table(indexes[1])[7]

sum(sapply(indexes, function(ind){
  sum(ind == 3)
}))

set.seed(1)
q_75 <- replicate(10000, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q_75)
sd(q_75)

set.seed(1)
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  quantile(y[ind], 0.75)
})
mean(q_75_star)
sd(q_75_star)

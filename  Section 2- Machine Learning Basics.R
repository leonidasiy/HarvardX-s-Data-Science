# 2.1: Basics of Evaluating Machine Learning Algorithms
library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

sum(dat$sex[x == "inclass"] == "Female") / sum(x == "inclass")
sum(dat$sex[x == "online"] == "Female") / sum(x == "online")

y_hat <- ifelse(x == "inclass", "Female", "Male") %>% factor(levels = levels(y))
y_hat
mean(y_hat == dat$sex)

table(y_hat, y)

sensitivity(y_hat, y)

specificity(y_hat, y)

mean(y == "Female")



library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(76)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test_index
test <- iris[test_index,]
train <- iris[-test_index,]
names(train[,-5][1])[1]

f <- function(col) {
  cutoff <- seq(min(col), max(col), 0.1)
  accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(col > x, "virginica", "versicolor") %>%
      factor(levels = levels(test$Species))
    mean(y_hat == train$Species)
  })
  #max(accuracy)
  accuracy
}

best_cutoff <- sapply(train[,-5], f)
best_cutoff

cutoff <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)[which.max(f(train$Petal.Width))]
cutoff

y_hat <- ifelse(test$Petal.Width > cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

plot(iris, pch=21, bg=iris$Species)
cutoff2 <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)[which.max(f(train$Petal.Length))]
cutoff2
y_hat <- ifelse(test$Petal.Width > cutoff & test$Petal.Length > cutoff2, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)



# 2.2: Conditional Probabilities
library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

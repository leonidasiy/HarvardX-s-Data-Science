# 6.1: Case Study: MNIST
models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")

library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

models_y_hat <- sapply(fits, function(fit_model){
  predict(fit_model, mnist_27$test)
})
dim(models_y_hat)

model_accuracies <- apply(models_y_hat == mnist_27$test$y, 2, mean)
model_accuracies
mean(model_accuracies)

df <- as.data.frame(table(models_y_hat[1,]), stringsAsFactors =TRUE)
df

y_hat_maj <- sapply(seq(1,nrow(models_y_hat)), function(index_line) {
  df <- as.data.frame(table(models_y_hat[index_line,]))
  df[which.max(df$Freq),]$Var1
})
y_hat_maj
mean(y_hat_maj == mnist_27$test$y)

acc_hat <- sapply(fits, function(fit) {min(fit$results$Accuracy)})
mean(acc_hat)

ind <- acc_hat >= 0.8
votes <- rowMeans(models_y_hat[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)



# 6.2: Recommendation Systems
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

med_ratings <- movielens %>%
  group_by(year) %>%
  summarize(med=median(rating))
med_ratings$year[which.max(med_ratings$med)]
med_ratings

res2 <- movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(avg_rating = mean(rating), n = n(), title=title[1], years=2018 - first(year)) %>%
  mutate(n_year = n / years) %>%
  top_n(25, n_year) %>%
  arrange(desc(n_year))
res2

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            avg_rating = mean(rating)) %>%
  mutate(n_year = n / years) %>%
  ggplot(aes(n_year, avg_rating)) +
  geom_point() +
  geom_smooth()

movielens <- mutate(movielens, date = as_datetime(timestamp))

movielens %>% mutate(week = round_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(week, avg_rating)) +
  geom_point() +
  geom_smooth()

movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# 6.3: Regularization
options(digits=7)

set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

schools_top10 <- schools %>%
  top_n(10, score) %>% 
  arrange(desc(score)) %>%
  select(id, size, score)
schools_top10

median(schools$size)
median(schools_top10$size)

schools %>%
  top_n(-10, score) %>% 
  arrange(desc(score)) %>%
  select(id, size, score) %>%
  summarize(median(size))

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2) 

overall <- mean(sapply(scores, mean))

alpha <- 25
schools_alpha <- schools %>%
  mutate(score_dev = overall + (score - overall) * size / (size + alpha)) %>%
  arrange(desc(score_dev)) %>%
  top_n(10, score_dev)
schools_alpha

alphas <- seq(10, 250)
rmses <- sapply(alphas, function(alpha){
  schools %>%
    mutate(score_dev = overall + (score - overall) * size / (size + alpha)) %>%
    summarize(rmse = sqrt(1/1000 * sum((score_dev-quality)^2))) %>%
    pull(rmse)
})
alphas[which.min(rmses)]

alpha <- alphas[which.min(rmses)]
schools_best_alpha <- schools %>%
  mutate(score_dev = overall + (score - overall) * size / (size + alpha)) %>%
  arrange(desc(score_dev)) %>%
  top_n(10, score_dev)
schools_best_alpha

rmses <- sapply(alphas, function(alpha){
  schools %>%
    mutate(score_dev = overall + (score) * size / (size + alpha)) %>%
    summarize(rmse = sqrt(1/1000 * sum((score_dev-quality)^2))) %>%
    pull(rmse)
})
alphas[which.min(rmses)]

rmses <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
alphas[which.min(rmses)]



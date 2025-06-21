# 3.1: Linear Regression for Prediction
library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rmses <- replicate(n, {
  indices <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_dat <- slice(dat, -indices)
  test_dat <- slice(dat, indices)
  model <- lm(y ~ x, data=train_dat)
  pred <- predict(model, test_dat)
  RMSE(pred, test_dat$y)
})
mean(rmses)
sd(rmses)

f <- function(n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmses <- replicate(100, {
    indices <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_dat <- slice(dat, -indices)
    test_dat <- slice(dat, indices)
    model <- lm(y ~ x, data=train_dat)
    pred <- predict(model, test_dat)
    RMSE(pred, test_dat$y)
  })
  c(mean(rmses), sd(rmses))
}

set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
sapply(n, f)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rmses <- replicate(n, {
  indices <- createDataPartition(dat$y, list = FALSE)
  train_dat <- slice(dat, -indices)
  test_dat <- slice(dat, indices)
  model <- lm(y ~ x, data=train_dat)
  pred <- predict(model, test_dat)
  RMSE(pred, test_dat$y)
})
mean(rmses)
sd(rmses)

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
indices <- createDataPartition(dat$y, list = FALSE)
train_dat <- slice(dat, -indices)
test_dat <- slice(dat, indices)
model1 <- lm(y ~ x_1, data=train_dat)
model2 <- lm(y ~ x_2, data=train_dat)
model3 <- lm(y ~ x_1 + x_2, data=train_dat)
pred1 <- predict(model1, test_dat)
pred2 <- predict(model2, test_dat)
pred3 <- predict(model3, test_dat)
RMSE(pred1, test_dat$y)
RMSE(pred2, test_dat$y)
RMSE(pred3, test_dat$y)

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
indices <- createDataPartition(dat$y, list = FALSE)
train_dat <- slice(dat, -indices)
test_dat <- slice(dat, indices)
model1 <- lm(y ~ x_1, data=train_dat)
model2 <- lm(y ~ x_2, data=train_dat)
model3 <- lm(y ~ x_1 + x_2, data=train_dat)
pred1 <- predict(model1, test_dat)
pred2 <- predict(model2, test_dat)
pred3 <- predict(model3, test_dat)
RMSE(pred1, test_dat$y)
RMSE(pred2, test_dat$y)
RMSE(pred3, test_dat$y)



# 3.2: Smoothing
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

span <- 60 / diff(range(dat$date))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, date)) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = 2)

dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

library(broom)
library(dslabs)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

qplot(x_2, y, data = mnist_27$train)

loess_fit <- mnist_27$train %>% mutate(y = ifelse(y == "7", 1, 0)) %>% loess(y ~ x_2, data=., degree=1)
test_data <- mnist_27$test %>% 
  mutate(y = ifelse(y == "7", 1, 0))
pred <- ifelse(predict(loess_fit, newdata=test_data) > 0.5, 1, 0)
pred
mean(test_data$y == pred, na.rm=TRUE)

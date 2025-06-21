# 5.1: Caret Package
library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}
ind <- which(pvals < 0.01)
length(ind)

set.seed(1)
x_subset <- x[ , ind]

fit <- train(x_subset, y, method = "glm")
fit$results

set.seed(1)
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ ., data = dat)
fit

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2) # 2 = "red"

library(randomForest)
fit <- randomForest(y ~ x, data = dat)
dat %>% 
mutate(y_hat = predict(fit)) %>% 
ggplot() +
geom_point(aes(x, y)) +
geom_step(aes(x, y_hat), col = "red")

plot(fit)

library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% 
mutate(y_hat = predict(fit)) %>% 
ggplot() +
geom_point(aes(x, y)) +
geom_step(aes(x, y_hat), col = "red")


library(caret)
library(dslabs)
set.seed(1991)
data("tissue_gene_expression")

fit1 <- with(tissue_gene_expression, 
             train(x, y, method = "rpart",
                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
plot(fit1)

set.seed(1991)
fit2 <- with(tissue_gene_expression, 
             train(x, y, method = "rpart", control = rpart.control(minsplit = 0),
                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
plot(fit2)
max(fit2$results$Accuracy)

plot(fit2$finalModel, margin=0.1)
text(fit2$finalModel, cex=0.5)

library(randomForest)
set.seed(1991)
fit <- with(tissue_gene_expression, 
             train(x, y, method = "rf", tuneGrid = data.frame(mtry = seq(50, 200, 25)),  nodesize =1))
plot(fit)
fit$results$mtry[which.max(fit$results$Accuracy)]



# 5.2: Titanic Exercises
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean[-test_index,]
test_set <- titanic_clean[test_index,]
nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)

set.seed(3)
pred <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(test_set$Survived == pred)

train_set %>%
  group_by(Sex) %>%
  summarize(prop = mean(Survived == 1))

pred1 <- with(test_set, ifelse(Sex == "female", 1, 0))
mean(test_set$Survived == pred1)

train_set %>%
  group_by(Pclass) %>%
  summarize(prop = mean(Survived == 1))

pred2 <- with(test_set, ifelse(Pclass == 1, 1, 0))
mean(test_set$Survived == pred2)

survival_class <- train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(prop = ifelse(mean(Survived == 1) > 0.5, 1, 0))
survival_class

pred3 <- with(test_set, ifelse((Pclass == 1 | Pclass == 2) & Sex == "female", 1, 0))
mean(test_set$Survived == pred3)

confusionMatrix(data=factor(pred1), reference=factor(test_set$Survived))
confusionMatrix(data=factor(pred2), reference=factor(test_set$Survived))
confusionMatrix(data=factor(pred3), reference=factor(test_set$Survived))

F_meas(data=factor(pred1), reference=factor(test_set$Survived))
F_meas(data=factor(pred2), reference=factor(test_set$Survived))
F_meas(data=factor(pred3), reference=factor(test_set$Survived))


set.seed(1)
model <- train(Survived ~ Fare, data = train_set, method = 'gamLoess')
Survived_hat <- predict(model, test_set)
mean(test_set$Survived == Survived_hat)

set.seed(1)
model <- train(Survived ~ Age, data = train_set, method = 'glm')
Survived_hat <- predict(model, test_set)
mean(test_set$Survived == Survived_hat)
set.seed(1)
model <- train(Survived ~ Sex+Pclass+Fare+Age, data = train_set, method = 'glm')
Survived_hat <- predict(model, test_set)
mean(test_set$Survived == Survived_hat)
set.seed(1)
model <- train(Survived ~ ., data = train_set, method = 'glm')
Survived_hat <- predict(model, test_set)
mean(test_set$Survived == Survived_hat)

set.seed(6)
model <- train(Survived ~ ., data=train_set, method = "knn", tuneGrid=data.frame(k=seq(3, 51, 2)))
model$results$k[which.max(model$results$Accuracy)]
plot(model)

pred <- predict(model, test_set)
mean(test_set$Survived == pred)

set.seed(8)
model <- train(Survived ~ ., 
                   data=train_set, 
                   method = "knn",
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   trControl = trainControl(method = "cv", number=10, p=0.9))
model$results$k[which.max(model$results$Accuracy)]
pred <- predict(model, test_set)
mean(test_set$Survived == pred)

set.seed(10)
model <- train(Survived ~ ., 
                     data=train_set, 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
model$results$cp[which.max(model$results$Accuracy)]
pred <- predict(model, test_set)
mean(test_set$Survived == pred)

plot(model$finalModel, margin=0.1)
text(model$finalModel)

set.seed(14)
model <- train(Survived ~., 
                  data = train_set,
                  method = "rf", 
                  tuneGrid = data.frame(mtry = seq(1, 7)), 
                  ntree = 100)
model$results$mtry[which.max(model$results$Accuracy)]
pred <- predict(model, test_set)
mean(test_set$Survived == pred)

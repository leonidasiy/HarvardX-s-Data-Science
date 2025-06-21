options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

#2
titanic %>% ggplot(aes(Age, ..count.., fill=Sex)) +
  geom_density(bw=1.0, alpha=0.2, position="stack")
  
titanic %>% ggplot(aes(Age, fill=Sex)) +
  geom_density()

#3
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
titanic %>% ggplot(aes(sample=Age)) +
  geom_qq(dparams=params) +
  geom_abline()

#4
titanic %>% ggplot(aes(Survived, fill=Sex)) +
  geom_bar(position = position_dodge())

#5
titanic %>% ggplot(aes(Age, ..count.., fill=Survived)) +
  geom_density(alpha=0.2)

#6
titanic %>% filter(Fare!=0) %>% ggplot(aes(Survived, Fare)) +
  geom_boxplot(alpha=0.2) +
  scale_y_continuous(trans="log2") +
  geom_point() +
  geom_jitter()

#7
titanic %>% ggplot(aes(Pclass, fill=Survived)) +
  geom_bar()
titanic %>% ggplot(aes(Pclass, fill=Survived)) +
  geom_bar(position = position_fill())
titanic %>% ggplot(aes(Survived, fill=Pclass)) +
  geom_bar(position = position_fill())

#8
titanic %>% ggplot(aes(Age, ..count.., fill=Survived)) +
  geom_density(alpha=0.2) +
  facet_grid(Sex~Pclass)

library(HistData)
library(dplyr)
library(ggplot2)

# 2.2: Least Squares Estimates
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method='lm')

library(Lahman)
teams <- Teams %>% filter(yearID %in% 1961:2001) %>% mutate(BB_per_game = BB/G, R_per_game = R/G, HR_per_game=HR/G)
colnames(teams)

lm(teams$R_per_game ~ teams$BB_per_game + teams$HR_per_game)


set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

model <- lm(female_heights$mother ~ female_heights$daughter)

predict(model, data.frame(female_heights$mother[1]))

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_range <- Batting %>% filter(yearID %in% c(1999:2001)) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
colnames(bat_range)
sum(bat_range$mean_singles > 0.2, na.rm=TRUE)
sum(bat_range$mean_bb > 0.2, na.rm=TRUE)

bat_join <- inner_join(bat_02, bat_range, by='playerID')
summarize(bat_join, r_single=cor(singles, mean_singles), r_bb=cor(bb, mean_bb))

bat_join %>% ggplot(aes(mean_singles, singles)) + geom_point()
bat_join %>% ggplot(aes(mean_bb, bb)) + geom_point()

lm(bat_join$singles ~ bat_join$mean_singles)
lm(bat_join$bb ~ bat_join$mean_bb)



# 2.3: Advanced dplyr: summarize with functions and broom
library(tidyverse)
library(HistData)
library(broom)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

galton %>% group_by(pair) %>% mutate(n=n(), r=cor(childHeight, parentHeight)) %>% summarize(tidy(lm(childHeight~parentHeight, data=across()), conf.int = TRUE))



# 2.4: Regression and Baseball

model <- Teams %>% filter(yearID == 1971) %>% lm(R~BB + HR, data=.) %>% tidy()
model

model_all <- Teams %>% filter(yearID %in% c(1961:2018)) %>% group_by(yearID) %>% do(tidy(lm(R~BB + HR, data=.), conf.int=TRUE))
model_all %>% filter(term == 'BB') %>% ggplot(aes(yearID, estimate)) + geom_point()

tidy(lm(estimate~yearID, data=filter(model_all, term == 'BB')), conf.int=TRUE)

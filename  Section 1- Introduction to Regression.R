library(Lahman)
library(tidyverse)
library(dslabs)

colnames(Teams)

Teams %>% filter(yearID %in% 1961:2001) %>% mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
          ggplot(aes(AB_per_game, R_per_game)) + 
          geom_line()
summarize(Teams %>% filter(yearID %in% 1961:2001) %>% mutate(AB_per_game = AB/G, R_per_game = R/G), r=cor(AB_per_game, R_per_game))

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(win_rate = W / G, E_per_game = E / G) %>%
  ggplot(aes(win_rate, E_per_game)) + 
  geom_point(alpha = 0.5)
summarize(Teams %>% filter(yearID %in% 1961:2001) %>% mutate(win_rate = W / G, E_per_game = E / G), r=cor(win_rate, E_per_game))

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(triples_per_game = X3B / G, doubles_per_game = X2B / G) %>%
  ggplot(aes(triples_per_game, doubles_per_game)) + 
  geom_point(alpha = 0.5)
summarize(Teams %>% filter(yearID %in% 1961:2001) %>% mutate(triples_per_game = X3B / G, doubles_per_game = X2B / G), r=cor(triples_per_game, doubles_per_game))

set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies %>%     
    filter(gender == "female") %>%     
    group_by(family) %>%     
    sample_n(1) %>%     
    ungroup() %>%     
    select(mother, childHeight) %>%     
    rename(daughter = childHeight)
colnames(female_heights)
summarize(female_heights, mm=mean(mother), mstd=sd(mother), dm=mean(daughter), dstd=sd(daughter), r=cor(mother, daughter))
0.325 * 2.39/2.29
64.3 - 0.325 * 2.39/2.29 * 64.1
0.325**2
64.3 - 0.325 * 2.39/2.29 * 64.1 + 0.325 * 2.39/2.29 * 60

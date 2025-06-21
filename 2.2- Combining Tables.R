library(tidyverse)
library(dplyr)
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

People %>% as_tibble()

top_names <- top %>% left_join(People) %>%
  select(playerID, nameFirst, nameLast, HR)
head(top_names)

head(Salaries)
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

awards <- AwardsPlayers %>% filter(yearID == 2016) %>% 
  inner_join(top_names, by='playerID') %>% 
  select(playerID) %>%
  unique()
head(awards)
dim(awards)
non_top_10_awardees <- AwardsPlayers %>% filter(yearID == 2016) %>%
  inner_join(People, by='playerID') %>% 
  select(playerID)
head(non_top_10_awardees)
setdiff(non_top_10_awardees, awards)

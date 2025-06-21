library(gtools)
library(tidyverse)

#1
dim(permutations(8, 3))
dim(permutations(3, 3))
6 / 336
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1, sample.kind="Rounding")
results <- replicate(10000, {
  all(sample(runners, 3) == "Jamaica")
})
mean(results)

#2
dim(expand.grid(1:6, 1:6, 1:5, 1:2))[1] / 2
dim(expand.grid(1:6, 1:6, 1:5, 1:3))[1] / 2
dim(expand.grid(1:6, 1:6, 1:5, 1:4, 1:3))[1] / 6
meal_combs <- function(entrees){
  dim(expand.grid(1:entrees, 1:6, 1:5, 1:3))[1] / 2
}
sapply(1:12, meal_combs)
meal_combs <- function(sides){
  dim(expand.grid(1:6, 1:sides, 1:sides-1, 1:3))[1] / 2
}
sapply(2:12, meal_combs)

#3
head(esoph)
dim(esoph)
all_cases = sum(esoph$ncases)
all_controls = sum(esoph$ncontrols)

#4
unique(esoph$alcgp)
alcohol_highest <- esoph %>% filter(alcgp=="120+")
sum(alcohol_highest$ncases) / (sum(alcohol_highest$ncases) + sum(alcohol_highest$ncontrols))
alcohol_lowest <- esoph %>% filter(alcgp=="0-39g/day")
sum(alcohol_lowest$ncases) / (sum(alcohol_lowest$ncases) + sum(alcohol_lowest$ncontrols))
cases <- esoph %>% filter(ncases>0)
sum(filter(cases, tobgp!="0-9g/day")$ncases) / sum(cases$ncases)
controls <- esoph %>% filter(ncontrols>0)
sum(filter(controls, tobgp!="0-9g/day")$ncontrols) / sum(controls$ncontrols)

#5
sum(filter(cases, alcgp=="120+")$ncases) / sum(cases$ncases)
unique(esoph$tobgp)
sum(filter(cases, tobgp=="30+")$ncases) / sum(cases$ncases)
sum(filter(cases, alcgp=="120+" & tobgp=="30+")$ncases) / sum(cases$ncases)
sum(filter(cases, alcgp=="120+" | tobgp=="30+")$ncases) / sum(cases$ncases)

#6
sum(filter(controls, alcgp=="120+")$ncontrols) / sum(controls$ncontrols)
sum(filter(cases, alcgp=="120+")$ncases) / sum(cases$ncases) / (sum(filter(controls, alcgp=="120+")$ncontrols) / sum(controls$ncontrols))#0.225 / 0.0284
sum(filter(controls, tobgp=="30+")$ncontrols) / sum(controls$ncontrols)
sum(filter(controls, alcgp=="120+" & tobgp=="30+")$ncontrols) / sum(controls$ncontrols)
sum(filter(controls, alcgp=="120+" | tobgp=="30+")$ncontrols) / sum(controls$ncontrols)
0.33 / 0.0903

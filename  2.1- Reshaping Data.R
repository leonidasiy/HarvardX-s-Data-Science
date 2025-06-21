library(tidyverse)
library(dslabs)

head(co2)
colnames(CO2)
#view(co2)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
head(co2_wide)
co2_tidy <- pivot_longer(co2_wide, -year, names_to = "month", values_to = "co2")

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
head(dat, 10)
dat_tidy <- pivot_wider(dat, names_from = gender, values_from = admitted)

tmp <- admissions %>%
  pivot_longer(cols = c(admitted, applicants), names_to = "key", values_to = "value")
tmp
tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2

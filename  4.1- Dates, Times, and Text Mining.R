library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

dates <- c("09-01-02", "01-12-07", "02-03-04")

data(brexit_polls)
head(brexit_polls)
sum(month(brexit_polls$startdate) == 4)
sum(round_date(brexit_polls$enddate, unit='week') == '2016-06-12')

table(weekdays(brexit_polls$enddate))

data(movielens)
timestamps <- as_datetime(movielens$timestamp)
table(year(timestamps))
which.max(table(year(timestamps)))
which.max(table(hour(timestamps)))



gtb <- gutenberg_metadata
filter(gtb, str_detect(gtb$title, "Pride and Prejudice")) %>% summarise(n_distinct(gutenberg_id))

gtb <- gutenberg_works()
filter(gtb, str_detect(gtb$title, "Pride and Prejudice"))

words <- gutenberg_download(gutenberg_id=1342, mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg/")
words <- unnest_tokens(words, words, text) %>% filter(!words %in% stop_words$word & !str_detect(words, '\\d')) %>% select(words)
sum(table(words) > 100)
which.max(table(words))
max(table(words))

afinn <- get_sentiments("afinn")
colnames(afinn) <- c("words","value")
afinn_sentiments <- inner_join(words, afinn, by="words")
nrow(filter(afinn_sentiments, value > 0)) / nrow(afinn_sentiments)
nrow(filter(afinn_sentiments, value == 4))

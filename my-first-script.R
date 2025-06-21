library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dplyr)
library(dslabs)
library(data.table)
library(gtools)

p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

B <- 10000
p <- 0.04
x <- 0.05 * 180000

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

?seq
seq(-0.01, 0.01, length = 100)
seq(1,2,length=11)

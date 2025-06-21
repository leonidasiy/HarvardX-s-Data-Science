#1
cor <- 1 / 5
incor <- 1 - cor
cor * 1 + incor * -0.25
n <- 44
mu <- n * (cor * 1 + incor * -0.25)
sigma <- sqrt(n) * (1 - -0.25) * sqrt(cor * incor)
1 - pnorm(8, mu, sigma)
set.seed(21, sample.kind = "Rounding")
S <- replicate(10000, {
  sample(c(1, -0.25), 44, c(cor, incor), replace=TRUE)
})
1 - pnorm(8, mean(S), mean(sigma))

#2
cor <- 1 / 4
incor <- 1 - cor
prob_35 <- function(cor){
  incor <- 1 - cor
  mu <- n * (cor * 1 + incor * 0)
  sigma <- sqrt(n) * (1 - 0) * sqrt(cor * incor)
  1 - pnorm(35, mu, sigma)
}
p <- seq(0.25, 0.95, 0.05)
sapply(p, prob_35)
0.25 + 12 * 0.05

#3
los <- 33/38
suc <- 5/38
n <- 500
mu <- los * -1 + suc * 6
abs(-1 - 6) * sqrt(los * suc)
sigma <- (abs(-1 - 6) * sqrt(los * suc)) / sqrt(n)
n * (los * -1 + suc * 6)
sqrt(n) * (abs(-1 - 6) * sqrt(los * suc))
pnorm(0, mu, sigma)

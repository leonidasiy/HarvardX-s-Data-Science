set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)

#1
mu <- mean(act_scores)
sigma <- sd(act_scores)
sum(act_scores >= 36)
mean(act_scores > 30)
mean(act_scores <= 10)

#2
x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

#3
z_scores <- (act_scores - mu) / sigma
mean(z_scores > 2)
2 * sigma + mu
qnorm(0.975, mu, sigma)

#4
cdf_func <- function(x){
  pnorm(x, mu, sigma)
}
sapply(1:36, cdf_func)
qnorm(0.95, 20.9, 5.7)
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
sample_quantiles
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
plot(theoretical_quantiles, sample_quantiles)
abline(lm(sample_quantiles ~ theoretical_quantiles))

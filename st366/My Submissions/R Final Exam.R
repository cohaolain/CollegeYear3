# Ciarán Ó hAoláin - 17309103

# Imports
require(MASS)
require(microbenchmark)

# Q1
## (a)
? anorexia
q1_dataset <- anorexia
q1_dataset$difference <- anorexia$Postwt - anorexia$Prewt

## (b)
tapply(q1_dataset$difference, q1_dataset$Treat, median)
# Family Treatment appears to be the most effective

## (c)
hist(
  q1_dataset$difference,
  freq = FALSE,
  ylim = c(0, 0.055),
  xlim = c(-20, 30)
)
lines(density(q1_dataset$difference, bw = 3))

## (d)
mu <- mean(q1_dataset$difference)
sigma <- sd(q1_dataset$difference)
curve(
  dnorm(x, mean = mu, sd = sigma),
  col = "red",
  add = TRUE,
  lwd = 3
)

## (e)
qq <- qqnorm(q1_dataset$difference, plot = TRUE)
qqline(q1_dataset$difference)
cor(qq$x, qq$y)
# Result is 0.9889619, which indicates a strong positive correlation.
# This suggests (crudely) the validity of the assumption of normality.


# Q2
## (a)
X <-
  matrix(c(1, 0, 2, 1, 5, 1, 1, 4, 2, 6, 2, 1, 2, 3, 7),
         ncol = 5,
         byrow = TRUE)
X

## (b)
? svd
X_svd <- svd(X)

## (c)
U <- X_svd$u
V <- X_svd$v

X_svd$d
D_plus <- matrix(0, nrow = 3, ncol = 3)
diag(D_plus) <- 1 / X_svd$d
D_plus

## (d)
Z <- V %*% D_plus %*% sapply(seq(nrow(U)), function(i)
  U[i, ])
Z

## (e)
all.equal(ginv(X), Z)
all.equal(X %*% Z %*% X, X)
# Both true


# Q3
set.seed(17309103)
? rt
q3_sample <- rt(10000, df = 3)

## (a)
hist(q3_sample, freq = FALSE, breaks = "Scott")

## (b)
mu_hat <- mean(q3_sample)
sigma_hat <- sd(q3_sample)

## (c)
get_outliers <- function(x, crit = qnorm(0.975)) {
  ll <- mu_hat - crit * sigma_hat
  ul <- mu_hat + crit * sigma_hat
  which(x < ll | x > ul)
}
length(get_outliers(q3_sample)) / length(q3_sample)
# 4.35% lie outside the range.

## (d)
hist(q3_sample[-get_outliers(q3_sample)], freq = FALSE, breaks = "Scott")


# Q4
## (a)
rdirichlet1 <- function(alpha, K = length(alpha)) {
  variates <- rgamma(K, alpha, rate = 1)
  return(variates / sum(variates))
}

## (b)
all.equal(sum(rdirichlet1(1, K = 10)), 1)
# TRUE
all.equal(sum(rdirichlet1(c(2, 5, 1, 25, 66))), 1)
# TRUE also

## (c)
rdirichlet2 <- function(alpha, K = length(alpha)) {
  if (K < 2)
    warning("K is < 2, K should be >= 2")
  if (length(alpha) != K && length(alpha) != 1)
    return(stop("If alpha is a vector, K must be the length of alpha"))
  variates <- rgamma(K, alpha, rate = 1)
  return(variates / sum(variates))
}

## (d)
system.time(replicate(100000, rdirichlet2(seq(0.5, 5, by = 0.5))))
# elapsed: 2.268 seconds

## (e)
rdirichlet3 <- function(alpha, K = length(alpha)) {
  if (K < 2)
    warning("K is < 2, K should be >= 2")
  if (length(alpha) != K && length(alpha) != 1)
    return(stop("If alpha is a vector, K must be the length of alpha"))
  # Optimise if distribution is symmetric uniform
  if (all(alpha == 1)) {
    variates <- rexp(K)
  } else {
    variates <- rgamma(K, alpha, rate = 1)
  }
  return(variates / sum(variates))
}
? microbenchmark
x <-
  microbenchmark(rdirichlet2(1, K = 10),
                 rdirichlet3(1, K = 10),
                 times = 100000)
medians <- summary(x)$median
# 4.594 microseconds for rdirichlet2,
# 3.412 microseconds for rdirichlet3
medians[2] / medians[1]
# 0.7427079
# It takes about 26% less time to run rdirichlet3 in this special case,
# a significant improvement.

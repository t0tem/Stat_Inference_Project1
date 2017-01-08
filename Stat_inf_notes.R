##########################################################
###               motivating example                   ###

hist(runif(1000))
mns = NULL
for (i in 1 : 10000) mns = c(mns, mean(runif(40)))
hist(mns)

##########################################################





lambda <- 0.2
nosim <- 1000
n <- 40

mu <- 1/lambda
sigma <- 1/lambda

set.seed(11142)

sample <- matrix(rexp(nosim * n, rate = lambda), nosim)
sample_means <- apply(sample, 1, mean)
sample_vars <- apply(sample, 1, var)

hist(sample_means)
mean(sample_var)


## applying Central Limit Theorem

###function for calculation of test statistic

my_func <- function(x, n) {sqrt(n) * (mean(x) - mu) / sigma}
my_dat <- apply(matrix(rexp(nosim * 40, rate = lambda), nosim), 1, my_func, 40)

hist(my_dat)
mean(my_dat)
hist(rnorm(1000))


######################################################### functions from Brian's lecture

cfunc <- function(x, n) sqrt(n) * (mean(x) - 3.5) / 1.71

dat <- data.frame(
      x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), nosim), 1, cfunc, 10),
            apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE), nosim), 1, cfunc, 20),
            apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE), nosim), 1, cfunc, 30)
      ),
      size = factor(rep(c(10, 20, 30), rep(nosim, 3))))

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

# making normalized data
my_dat <- apply(sample, 1, my_func, 40)
my_rand <- rexp(1000, 0.2)

#making global vector (+ 1000 random exponentials) 
r_dat <- c(my_dat, my_rand)

#factors for df
types <- factor(rep(1:2, each = 1000), labels = c("Average", "Random"))

#common df
df <- data.frame(types, r_dat)

#plotting with facets by factor
g3 <- ggplot(df, aes(x = r_dat)) + 
      geom_histogram(binwidth = 0.4, color = "white", fill = "steelblue1", aes(y = ..density..)) +
      labs(list(title = "Distribution of normalized test statistic", 
                x = element_blank(),
                y = element_blank())) +
      stat_function(fun = dnorm, size = 2, aes(color = "Standard Normal Distribution")) + 
      geom_density(size = 2, aes(color = "Test statistic density function")) + 
      scale_color_manual(values = c("blue4", "red"),
                         labels = c("Standard Normal Distribution",
                                  "Density function for normalized data"),
                         name = "") + 
      facet_grid(. ~ types)
      
      
print(g3)
# --> does not look good, x scale is not the same... better to plot one by one

## one by one 

# test statistic
g4 <- ggplot(data.frame(my_dat), aes(x = my_dat)) + 
      geom_histogram(binwidth = 0.4, color = "white", fill = "steelblue1", aes(y = ..density..)) +
      labs(list(title = "Distribution of normalized test statistic", 
                x = element_blank(),
                y = element_blank())) +
      stat_function(fun = dnorm, size = 2, aes(color = "Standard Normal Distribution")) + 
      geom_density(size = 2, aes(color = "Test statistic density function")) + 
      scale_color_manual(values = c("blue4", "red"),
                         labels = c("Standard Normal Distribution",
                                    "Density function for normalized data"),
                         name = "")
print(g4)

# random exps
g5 <- ggplot(data.frame(my_rand), aes(x = my_rand)) + 
      geom_histogram(binwidth = 1, color = "white", fill = "steelblue1", aes(y = ..density..)) +
      labs(list(title = "Distribution of normalized test statistic", 
                x = element_blank(),
                y = element_blank())) +
      stat_function(fun = dnorm, args = list(mean = 1/lambda, sd = 1/lambda), size = 2, aes(color = "Standard Normal Distribution")) + 
      geom_density(size = 2, aes(color = "Test statistic density function")) + 
      scale_color_manual(values = c("blue4", "red"),
                         labels = c("Normal Distribution",
                                    "Density function"),
                         name = "")
print(g5)

g6 <- ggplot(data.frame(sample_means), aes(x = sample_means)) + 
      geom_histogram(binwidth = 0.4, color = "white", fill = "steelblue1", aes(y = ..density..)) +
      labs(list(title = "Distribution of normalized test statistic", 
                x = element_blank(),
                y = element_blank())) +
      stat_function(fun = dnorm, args = list(mean = mean(sample_means), sd = sd(sample_means)), size = 2, aes(color = "Standard Normal Distribution")) + 
      geom_density(size = 2, aes(color = "Test statistic density function")) + 
      scale_color_manual(values = c("blue4", "red"),
                         labels = c("Normal Distribution",
                                    "Density function"),
                         name = "")
print(g6)



######################################################### functions from Brian's lecture

cfunc <- function(x, n) sqrt(n) * (mean(x) - 3.5) / 1.71

dat <- data.frame(
      x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), nosim), 1, cfunc, 10),
            apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE), nosim), 1, cfunc, 20),
            apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE), nosim), 1, cfunc, 30)
      ),
      size = factor(rep(c(10, 20, 30), rep(nosim, 3))))

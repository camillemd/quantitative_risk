# Quantitative Risk Management - Homework 2
# 09.05.2020
# Raphael Attali, Camille Morand-Duval

#### INITIALISATION ####

# set dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# libraries
library(ggplot2)

# set seed for reproducible results 
set.seed(1)

#### QUESTION 1 ####

# define mean and variance
mu = 3
sigma = 3
sample_size = 10000
  
# normal distribution
normal = rnorm(sample_size, mu, sigma)

# plot
gaussian = tibble(normal)
right = -6
left = 12
data_theo = tibble(x = seq(right, left, 0.1), 
                   y = dnorm(seq(right, left, 0.1), mu, sigma))
ggplot(gaussian, aes(x = normal, y = ..density..)) +
  geom_histogram(bins = 50) +
  geom_line(aes(x = x, y = y), data = data_theo, color = 'red')

sprintf('The mean of the normal distribution is: %f', mean(normal))
sprintf('True value is : %d', mu)
sprintf('The variance of the normal distribution is: %f', var(normal))
sprintf('True value is : %d', sigma^2)
  
# exponential distribution
exponential = rexp(sample_size, rate = 1 / mu) # rate = 1 / mean

# plot
exp_disp = tibble(exponential)
data_theo = tibble(x = seq(right, left, 0.1), y = dexp(seq(right, left, 0.1), rate = 1 / mu))
ggplot(exp_disp, aes(x = exponential, y = ..density..)) +
  geom_histogram(bins = 50) +
  geom_line(aes(x = x, y = y), data = data_theo, color = 'red')

sprintf('The mean of the normal distribution is: %f', mean(exponential))
sprintf('True value is : %d', mu)
sprintf('The variance of the normal distribution is: %f', var(exponential))
sprintf('True value is : %d', sigma^2)

#### QUESTION 2 ####

mu = 3
sigma = 3
N = c(5, 50, 500, 5000, 1000)

mean_exp = c()
variance_exp = c()
i = 1

for (n in N){
  
  # compute exponential
  exponential = rexp(n, rate = 1 / mu) # rate = 1 / mean

  # mean and variance
  mean_exp[c(i)] = mean(exponential)
  variance_exp[c(i)] = var(exponential)
  
  # index
  i = i + 1
}

mean_exp = data.frame(mean_exp)
variance_exp = data.frame(variance_exp)

ggplot(data.frame(mean_exp), aes(mean_exp)) + 
  geom_line()

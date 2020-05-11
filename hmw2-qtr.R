# Quantitative Risk Management - Homework 2
# 09.05.2020
# Raphael Attali, Camille Morand-Duval

#### INITIALISATION ####

# set dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# libraries
library(ggplot2)
library(reliaR)
library(tibble)

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
left = -6
right = 12
data_theo = tibble(x = seq(left, right, 0.1), 
                   y = dnorm(seq(left, right, 0.1), mu, sigma))

ggplot(gaussian, aes(x = normal, y = ..density..)) +
  geom_histogram(bins = 50) +
  geom_line(aes(x = x, y = y), data = data_theo, color = 'red')
ggsave('./plots/q1Norm.png')

sprintf('The mean of the normal distribution is: %f', mean(normal))
sprintf('True value is : %d', mu)
sprintf('The variance of the normal distribution is: %f', var(normal))
sprintf('True value is : %d', sigma^2)
  
# exponential distribution
exponential = rexp(sample_size, rate = 1 / mu) # rate = 1 / mean

# plot
exp_disp = tibble(exponential)
data_theo = tibble(x = seq(left, right, 0.1), y = dexp(seq(left, right, 0.1), rate = 1 / mu))
ggplot(exp_disp, aes(x = exponential, y = ..density..)) +
  geom_histogram(bins = 50) +
  geom_line(aes(x = x, y = y), data = data_theo, color = 'red')
ggsave('./plots/q1Exp.png')

sprintf('The mean of the normal distribution is: %f', mean(exponential))
sprintf('True value is : %d', mu)
sprintf('The variance of the normal distribution is: %f', var(exponential))
sprintf('True value is : %d', sigma^2)

#### QUESTION 2 ####

mu = 3
sigma = 3
N = c(5, 50, 500, 5000, 10000)

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

mean_exp = tibble(N, mean_exp)
ggplot(mean_exp, aes(N, mean_exp, color = 'Empirical')) + 
  geom_point() + 
  geom_line(aes(x = N, y = mu, color = 'Real')) + 
  labs(x = 'Sample size', y = 'Mean') + 
  scale_color_manual(name="Legend", values = c("black","red")) + 
  theme(legend.position = c(0.985, 0.015), legend.justification = c("right", "bottom"))
ggsave('./plots/q2Mean.png')

variance_exp = tibble(N, variance_exp)
ggplot(variance_exp, aes(N, variance_exp, color = 'Empirical')) + 
  geom_point() + 
  geom_line(aes(x = N, y = sigma^2, color = 'Real')) + 
  labs(x = 'Sample size', y = 'Variance') + 
  scale_color_manual(name="Legend", values = c("black","red")) + 
  theme(legend.position = c(0.985, 0.015), legend.justification = c("right", "bottom"))
ggsave('./plots/q2Var.png')

#### QUESTION 3 ####

# initialisation
observations = 10000
samples = 10000

mu = 1
sigma = 1

norm_max = c()
exp_max = c()

# generate maxima
for (i in 1:samples) {
  gen_data = rnorm(sample_size, mu, sigma)
  norm_max[c(i)] = max(gen_data)
  gen_data = rexp(sample_size, rate = 1 / mu)
  exp_max[c(i)] = max(gen_data)
}

# plot cumulative
df <- data.frame(x = c(exp_max, norm_max, rgumbel(sample_size, mu, sigma)), 
                 g = gl(3, samples))

ggplot(df, aes(x, colour = g)) + 
  stat_ecdf() +
  labs(x = 'Maxima', y = 'Cumulative Density') + 
  scale_color_manual(name="Legend", labels=c("Exponential", "Normal", "Gumbel"), 
                    values = c('blue', 'red', 'black')) + 
  theme(legend.position = c(0.985, 0.015), legend.justification = c("right", "bottom"))
ggsave('./plots/q3-cumul.png')

# compute densities for plot
density_exponential = density(exp_max)
density_normal = density(norm_max)

left = -10 #min(min(exp_max), min(norm_max))
right = max(max(exp_max), max(norm_max))

gumbel = tibble(x = seq(left, right, 0.1), y = dgumbel(seq(left, right, 0.1), mu, sigma))

ggplot(tibble(density_normal$x, density_normal$y), aes(density_normal$x, density_normal$y, color = 'Normal')) + 
  geom_line() + 
  geom_line(aes(x = density_exponential$x, y = density_exponential$y, color = 'Exponential')) + 
  geom_line(data = gumbel, aes(x = x, y = y, color = 'Gumbel')) +
  labs(x = 'Maxima', y = 'Density') + 
  scale_color_manual(name="Legend", values = c("blue", "black", "red")) + 
  theme(legend.position = c(0.985, 0.985), legend.justification = c("right", "top"))
ggsave('./plots/q3-densities.png')
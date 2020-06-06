# Quantitative Risk Management - Homework 2
# 09.05.2020
# Revised: 05.06.2020
# Raphael Attali, Camille Morand-Duval

#### INITIALISATION ####

# set dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# libraries
library(ggplot2)
library(reliaR)
library(tibble)
library(ggpubr)
library(Rmisc)

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

#### QUESTION 2 - REVISED ####

## normal case ##
mu = 3
sigma = 3
N = c(5, 50, 500, 5000, 10000)

mean_norm = c()
variance_norm = c()
i = 1

for (n in N){
  
  # compute exponential
  normal = rnorm(n, mu, sigma) # rate = 1 / mean
  
  # mean and variance
  mean_norm[c(i)] = mean(normal)
  variance_norm[c(i)] = var(normal)
  
  # index
  i = i + 1
}

mean_norm = tibble(N, mean_norm)
ggplot(mean_norm, aes(N, mean_norm, color = 'Empirical')) + 
  geom_point() + 
  geom_line(aes(x = N, y = mu, color = 'Real')) + 
  labs(x = 'Sample size', y = 'Mean') + 
  scale_color_manual(name="Legend", values = c("black","red")) + 
  theme(legend.position = c(0.985, 0.015), legend.justification = c("right", "bottom"))
ggsave('./plots/q2Mean-Norm.png')

variance_norm = tibble(N, variance_norm)
ggplot(variance_norm, aes(N, variance_norm, color = 'Empirical')) + 
  geom_point() + 
  geom_line(aes(x = N, y = sigma^2, color = 'Real')) + 
  labs(x = 'Sample size', y = 'Variance') + 
  scale_color_manual(name="Legend", values = c("black","red")) + 
  theme(legend.position = c(0.985, 0.985), legend.justification = c("right", "top"))
ggsave('./plots/q2Var-Norm.png')

#### FOLLOW-UP ####

## NORMAL ##

# HISTOGRAMS

# plot-function(histogram and densities)
plot_density_normal = function(normal, name){
  
  gaussian = tibble(normal)
  left = -6
  right = 12
  data_theo = tibble(x = seq(left, right, 0.1), 
                     y = dnorm(seq(left, right, 0.1), mu, sigma))
  
  plot = ggplot(gaussian, aes(x = normal, y = ..density..)) +
    geom_histogram(bins = 50) +
    geom_line(aes(x = x, y = y), data = data_theo, color = 'red') + 
    labs(x = name , y = 'Density')
  
  return(plot)
}

# sample
N = c(5, 50, 500, 1000, 5000, 10000)

# plot loop
plots <- list()  # new empty list
for (i in 1:6) {
  n = N[i]
  
  normal = rnorm(n, mu, sigma)
  p1 = plot_density_normal(normal, paste('n =', n))
  plots[[i]] <- p1  # add each plot into plot list
}
multiplot(plotlist = plots, cols = 3)
ggsave('./plots/normal-hist.png')

# ILLUSTRATION OF THE CLT

# density plot function 
plot_density = function(distribution, name){
  
  dist = tibble(distribution)
  left = -6
  right = 12
  data_theo = tibble(x = seq(left, right, 0.1), 
                     y = dnorm(seq(left, right, 0.1), mu, sigma))
  
  plot = ggplot(dist, aes(x = distribution, y = ..density..)) +
    geom_histogram(bins = 20) +
    geom_line(aes(x = x, y = y), data = data_theo, color = 'red') + 
    labs(x = name , y = 'Density')
  
  return(plot)
}

# initialisation
samples = 1000
mu = 1
sigma = 1
norm_mean = c()
exp_mean = c()

N = c(5, 50, 500, 1000, 5000, 10000)
plots_norm <- list()
plots_exp <- list()

for (i in 1:6){
  n = N[i]
  
  # generate means
  for (k in 1:samples) {
    gen_data = rnorm(n, mu, sigma)
    norm_mean[c(k)] = mean(gen_data)
    gen_data = rexp(n, rate = 1 / mu)
    exp_mean[c(k)] = mean(gen_data)
  }
  #show(plot_density(norm_mean, paste('n =', n)))
  
  density_normal = plot_density(norm_mean, paste('n =', n))
  plots_norm[[i]] = density_normal
  density_exp = plot_density(exp_mean, paste('n =', n))
  plots_exp[[i]] <- density_exp
}

multiplot(plotlist = plots_norm, cols = 3)
#ggsave('./plots/clt-normal-hist.png')
multiplot(plotlist = plots_exp, cols = 3)
#ggsave('./plots/clt-exp-hist.png')

#### QUESTION 3 ####

# initialisation
samples = 10000
sample_size = 10000

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

ggplot(tibble(density_normal$x, density_normal$y), aes(density_normal$x, density_normal$y)) + 
  geom_line(color = 'red') + 
  labs(x = 'Maxima', y = 'Density')
ggsave('./plots/q3-dens-norm.png')

ggplot(tibble(density_exponential$x, density_exponential$y), aes(x = density_exponential$x, y = density_exponential$y)) + 
  geom_line( color = 'blue') + 
 labs(x = 'Maxima', y = 'Density')
ggsave('./plots/q3-dens-exp.png')

ggplot(data = gumbel, aes(x = x, y = y)) + 
  geom_line( color = 'black') + 
  labs(x = 'Maxima', y = 'Density') 
ggsave('./plots/q3-dens-gumb.png')

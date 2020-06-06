# Quantitative Risk - Homework 3
# 05.06.2020
# Raphael Attali, Camille Morand-Duval

#### INITIALISATION ####

# set dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# libraries
library(ggplot2)
library(copula)
library(scatterplot3d)
library(mvtnorm)
library(randcorr)

# for reproducibility
set.seed(235)

#### QUESTION 1 ####

#### Q1 - SIMULATIONS ####

## normal dependence ##
samples = 10000
dim = 2

simulate_gaussian = function(samples, alpha, dim){
  P = matrix(0, dim, dim) + alpha # correlation matrix
  diag(P) = 1 # correlation matrix
  A = chol(P) # Cholesky decomposition of correlation matrix
  uniform_margin = matrix(nrow = samples, ncol = dim)# univariate standard normal
  gaussian_margin = matrix(nrow = samples, ncol = dim)# univariate standard normal
  
  for (i in 1:samples){
    Z = rnorm(dim)
    X = t(A) %*% Z
    uniform_margin[i,] = pnorm(X)
    gaussian_margin[i,] = qnorm(pnorm(X))
  }
  
  return(cbind(uniform_margin, gaussian_margin))
}

# alpha = 0.95
U = simulate_gaussian(samples, alpha = 0.95, dim)
uniform_margin = U[,1:2]
gaussian_margin = U[,3:4]

plot <- qplot(uniform_margin[,1], uniform_margin[,2], colour =  uniform_margin[,1], xlab = "U1", 
              ylab = "U2") + labs( colour = '')
print(plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
ggsave('./plots/q1-gaussian-p95.png')

plot <- qplot(gaussian_margin[,1], gaussian_margin[,2], colour =  U[,1], xlab = "U1", 
              ylab = "U2") + labs( colour = '')
print(plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
ggsave('./plots/q1-gaussian-p95-gmargin.png')

# alpha = 0.99
U = simulate_gaussian(samples, alpha = 0.99, dim)
uniform_margin = U[,1:2]
gaussian_margin = U[,3:4]

plot <- qplot(uniform_margin[,1], uniform_margin[,2], colour =  U[,1], xlab = "U1", 
              ylab = "U2") + labs( colour = '')
print(plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
ggsave('./plots/q1-gaussian-p99.png')

plot <- qplot(gaussian_margin[,1], gaussian_margin[,2], colour =  U[,1], xlab = "U1", 
              ylab = "U2") + labs( colour = '')
print(plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
ggsave('./plots/q1-gaussian-p99-gmargin.png')

## t dependence ##
samples = 10000
dim = 2
df = 3 # degrees of freedom

simulate_tstudent = function(samples, alpha, df, dim){
  P = matrix(0, dim, dim) + alpha # correlation matrix
  diag(P) = 1 # correlation matrix
  A = chol(P) # Cholesky decomposition of correlation matrix
  U = matrix(nrow = samples, ncol = dim) # univariate standard normal
  
  for (i in 1:samples){
    Z = rt(dim, df)
    X = t(A) %*% Z
    U[i,] = pt(X, df)
  }
  
  return(U)
}

# alpha = 0.7
# t dependence
U = simulate_tstudent(samples, alpha = 0.7, df, dim)
plot <- qplot(U[,1], U[,2], colour =  U[,1], xlab = "U1", #xlim = cbind(-6, 6), ylim = cbind(-6, 6),
              ylab = "U2") + labs( colour = '')
print(plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
ggsave('./plots/q1-tvalue-p70-gmargin.png')

# normal dependence
U = simulate_gaussian(samples, alpha = 0.7, dim)
plot <- qplot(U[,3], U[,4], colour =  U[,1], xlab = "U1", 
              ylab = "U2") + labs( colour = '')
print(plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
ggsave('./plots/q1-gaussian-p70.png')

#### QUESTION 2 ####

# clayton dependence
samples = 5000
U1 = runif(samples, 0, 1)
V2 = runif(samples, 0, 1)
U2 = c()
X1 = c()
X2 = c()

inverse.copula.U1.V2 = function(U1, V2, theta){
  U2 = (1 + U1^(-theta)*(V2^(-theta / (theta + 1))-1))^(-1/theta)
  return(U2)
}

for (i in 1:samples){
  #print(U1[i])
  U2[i] = inverse.copula.U1.V2(U1[i], V2[i], theta = 1.5)
  X1 = U1
  X2 = U2
}

plot <- qplot(X1, X2, colour =  X1, xlab = "X1", 
              ylab = "X2") + labs( colour = '')
print(plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
ggsave('./plots/q2-clayton.png')

#### APPENDIX ####

## Q1 - BUILT-IN FUNCTIONS ##

## normal dependence ##
dim = 2
param = 0.95

normal <- normalCopula(param, dim)
nrm <- rCopula(2000, normal)

# plot the samples
plot <- qplot(nrm[,1], nrm[,2], colour = nrm[,1], xlab = "Uniform margins", 
              ylab = "Gaussian Copula") + labs( colour = '')
print(plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
ggsave('./plots/q1-gaussian-p95.png')

## normal dependence ##
param = 0.99
normal <- normalCopula(param, dim)
nrm <- rCopula(2000, normal)

# plot the samples
plot <- qplot(nrm[,1], nrm[,2], colour = nrm[,1], xlab = "Uniform margins", 
              ylab = "Gaussian Copula") + labs( colour = '')
print(plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
ggsave('./plots/q1-gaussian-p99.png')

## t dependence ##
param = 0.7
dim = 3

tdistribution <- tCopula(param, dim)
tdist <- rCopula(2000, tdistribution)

# plot the samples
plot <- qplot(tdist[,1], tdist[,2], colour = tdist[,1], xlab = "Uniform margins", 
              ylab = "t-value Copula") + labs( colour = '')
print(plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
ggsave('./plots/q1-tvalue-p07.png')





















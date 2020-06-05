# Copula package
library(copula)
# Fancy 3D plain scatterplots
library(scatterplot3d)
# ggplot2
library(ggplot2)
# Useful package to set ggplot plots one next to the other
library(grid)
set.seed(235)

### Stack Overflow version ###
## Initialization and parameters 
set.seed(123)
P <- matrix(c(1, 0.1, 0.8,               # Correlation matrix
              0.1, 1, 0.4,
              0.8, 0.4, 1), nrow = 3)
d <- nrow(P)                             # Dimension
n <- 200                                 # Number of samples

## Simulation (non-vectorized version)
A <- t(chol(P))
U <- matrix(nrow = n, ncol = d)
for (i in 1:n){
  Z      <- rnorm(d) #X generates multivariate normal random variates in the space d.
  X      <- A%*%Z #Matrix multiplication.
  U[i, ] <- pnorm(X) # pnorm returns the cdf of X
}

## Simulation (compact vectorized version) 
#U <- pnorm(matrix(rnorm(n*d), ncol = d) %*% chol(P))

## Visualization
pairs(U, pch = 16,
      labels = sapply(1:d, function(i){as.expression(substitute(U[k], list(k = i)))}))

U

### Camille's version ###
# Generate a bivariate normal copula with rho = 0.7
normal <- normalCopula(param = 0.95, dim = 3)

# Generate random samples
nrm <- rCopula(2000, normal)

# Plot the samples
p1 <- qplot(nrm[,1], nrm[,2], colour = nrm[,1], xlab = "u", ylab = "v")
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))


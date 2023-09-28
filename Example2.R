#############################################################
# Generation of data 2 for Example 1 (beta0 = 0, p = 2)
#############################################################
n = 50 # sample size
beta = c(1, 0.5) # true coefficients vector
p = length(beta)
sigma = 0.4 # noise standard deviation

library(mnormt)
set.seed(234865) # set seed

# Generate matrix of covariates
Sigma = matrix(0.7, p, p) + diag(rep(1-0.7, p)) # covariance for design X
X = rmnorm(n, mean = rep(0, p), varcov = Sigma) # matrix of covariates

# Generate response Y
Y = X %*% beta + sigma * rnorm(n) 

# Verify has the right scaling
norm1 = as.numeric(crossprod(X[ , 1])/n) # should be 1
norm2 = as.numeric(crossprod(X[ , 2])/n) # should be 1

# Scaling of matrix of covariates X so that diag(X'X)/n has 1s
############################################################
# [ToDo] Create Xscaled from X that satisfies the above
Xscaled = X
Xscaled[, 1] = Xscaled[, 1]/sqrt(norm1)
Xscaled[, 2] = Xscaled[, 2]/sqrt(norm2)

crossprod(Xscaled[ , 1])/n
crossprod(Xscaled[ , 2])/n


# Coordinate descent implementation for the case p = 2, same as before but with scaled X
#############################################################
# Helper functions source
source("LassoFunctions.R")

niter = 50 # fixed number of iterations (for simplicity)
lambda = 0.2 # tuning parameter 
beta_start = rep(0, 2) # starting point 

# Apply coordinate descent
out = coordinateLasso(Xscaled, Y, beta_start, lambda, niter = niter)

plot(0:niter, out$fobj_vec)

# [ToDo] Transform out$beta so it can be applied to original X rather than Xscaled

# out$beta is beta for Xscaled
# I want beta for X
# And I know that each Xscaled[, j] = X[, j]/sqrt(normj)
# Xscaled[, 1] %*% out$beta[1] = (X[, 1]/sqrt(norm1)) %*% out$beta[1]
# = X[, 1] %*% (out$beta[1]/sqrt(norm1))

beta_unscaled = out$beta
beta_unscaled[1] = beta_unscaled[1]/sqrt(norm1)
beta_unscaled[2] = beta_unscaled[2]/sqrt(norm2)


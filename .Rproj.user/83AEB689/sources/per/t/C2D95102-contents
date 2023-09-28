# Generation of data for Example 3 (beta0 = 2, p = 2)
#############################################################
n = 50 # sample size
beta = c(1, 0.5) # true coefficients vector
beta0 = 2 # intercept, nonzero
p = length(beta)
sigma = 0.4 # noise standard deviation

library(mnormt)
set.seed(983645) # set seed

# Generate matrix of covariates
Sigma = matrix(0.7, p, p) + diag(rep(1-0.7, p)) # covariance for design X
X = rmnorm(n, mean = rep(0, p), varcov = Sigma) # matrix of covariates

# Generate response Y
Y = beta0 + X %*% beta + sigma * rnorm(n) 

# Centering and scaling
#############################################################
# [ToDo] Center both X and Y 
meanY = mean(Y)
Ycentered = Y - meanY
meansX = colMeans(X)
Xcentered = X - matrix(meansX, n, 2, byrow = TRUE)
# ALternative
Xcentered = scale(X, scale = FALSE)


# [ToDo] Scale centered X (so that n^{-1}X^tX has 1s on the diagonal)
norm1 = as.numeric(crossprod(Xcentered[ , 1])/n) # should be 1
norm2 = as.numeric(crossprod(Xcentered[ , 2])/n) # should be 1
Xscaled = Xcentered
Xscaled[, 1] = Xscaled[, 1]/sqrt(norm1)
Xscaled[, 2] = Xscaled[, 2]/sqrt(norm2)

crossprod(Xscaled[ , 1])/n
crossprod(Xscaled[ , 2])/n

# Coordinate descent implementation for the case p = 2
#############################################################
# Helper functions source
source("LassoFunctions.R")

niter = 50 # fixed number of iterations (for simplicity)
lambda = 0.5 # tuning parameter 
beta_start = rep(1, 2) # starting point 


# Apply coordinate descent on centered and scaled Xscaled, and centered Ycentered
out = coordinateLasso(Xscaled, Ycentered, beta_start, lambda, niter = niter)

# plot(0:niter, out$fobj_vec)
out2 = coordinateLasso2(Xscaled, Ycentered, beta_start, lambda, niter = niter)


# [ToDo] Perform back-centering and scaling to get intercept beta0 and vector of coefficients beta on the original scale
beta_unscaled = out$beta
beta_unscaled[1] = beta_unscaled[1]/sqrt(norm1)
beta_unscaled[2] = beta_unscaled[2]/sqrt(norm2)

# Intercept according to the formula
beta0 = meanY - meansX[1] * beta_unscaled[1] - meansX[2] * beta_unscaled[2]
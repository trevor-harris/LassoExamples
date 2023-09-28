
# Generation of data for Example 2 (beta0 = 2, p = 2)
#############################################################
n = 50 # sample size
beta = c(1, 0.5) # true coefficients vector
beta0 = 2 # intercept, nonzero
p = length(beta)
sigma = 0.4 # noise standard deviation

library(mnormt)
set.seed(983645) # set seed
# set.seed(234865) # set seed

# Generate matrix of covariates
Sigma = matrix(0.7, p, p) + diag(rep(1-0.7, p)) # covariance for design X
X = rmnorm(n, mean = rep(0, p), varcov = Sigma) # matrix of covariates

# Generate response Y
Y = beta0 + X %*% beta + sigma * rnorm(n) 

# Centering and scaling
#############################################################
# [ToDo] Center both X and Y 
meanY = mean(Y)
Ycentered = Y - mean(Y)
meansX = colMeans(X)

Xcentered = X - matrix(meansX, nrow(X), ncol(X), byrow = T)
# Xcentered = scale(X, scale = F)  # alternative way to center, but doesn't save it back
# (t(t(x) - meansX))

normsX = colSums(Xcentered^2)/n
Xtilde = Xcentered %*% diag(1/sqrt(normsX))

# [ToDo] Scale centered X (so that n^{-1}X^tX has 1s on the diagonal)


# Coordinate descent implementation for the case p = 2
#############################################################
# Helper functions source
source("LassoFunctions.R")

niter = 50 # fixed number of iterations (for simplicity)

# First set of parameters
lambda = 0.0 # tuning parameter 1
beta_start = rep(0, 2) # starting point 1

# [ToDo] Create a vector to store objective function values, and calculate the value of f(beta) at the beginning

fobj_vec = rep(0, niter)
for (i in 1:niter){
  # [ToDo] Update of beta1
  beta[1] = softthresh(crossprod(Xtilde[ ,1], Ycentered - Xtilde[, 2] * beta[2])/n, lambda)
  
  # [ToDo] Update of beta2
  beta[2] = softthresh(crossprod(Xtilde[ ,2], Ycentered - Xtilde[, 1] * beta[1])/n, lambda)
  
  # [ToDo] Calculate updated value of f(beta)
  fobj_vec[i] = lassoobj(Xtilde, Ycentered, beta, lambda)
}

beta

# [ToDo] Perform back-centering and scaling to get interecept beta0 and vector of coefficients beta on the original scale
beta_orig = diag(1/sqrt(normsX)) %*% beta
beta0 = meanY - sum(meansX * beta_orig)
beta0 = meanY - as.numeric(crossprod(meansX, beta_orig))

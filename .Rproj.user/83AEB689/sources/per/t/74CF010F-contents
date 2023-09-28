
# Generation of data for Example 1 (beta0 = 0, p = 2)
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

# # Scaling of matrix of covariates X
# #############################################################
# # [ToDo] scale X (so that n^{-1}X^tX has 1s on the diagonal)
# normsX = colSums(X^2)/n
# Xtilde = X %*% diag(1/sqrt(normsX))
Xtilde = X

# Coordinate descent implementation for the case p = 2
#############################################################
# Helper functions source
source("LassoFunctions.R")

niter = 100 # fixed number of iterations (for simplicity)

# First set of parameters
lambda = 0.0 # tuning parameter 1
beta_start = rep(0, 2) # starting point 1

# [ToDo] Create a vector to store objective function values, and calculate the value of f(beta) at the beginning

beta_hat = beta_start
fobj_vec = rep(0, niter)
for (i in 1:niter){
  # [ToDo] Update of beta1
  beta_hat[1] = softthresh(crossprod(Xtilde[ ,1], Y - Xtilde[, 2] * beta_hat[2])/n, lambda)
  
  # [ToDo] Update of beta2
  beta_hat[2] = softthresh(crossprod(Xtilde[ ,2], Y - Xtilde[, 1] * beta_hat[1])/n, lambda)
  
  # [ToDo] Calculate updated value of f(beta)
  fobj_vec[i] = lassoobj(Xtilde, Y, beta_hat, lambda)
}

plot(1:niter, fobj_vec)

beta_hat
beta

# [ToDo] Feel free to see how the results change with different set of parameters below
###################################################################################

# Second set of parameters
lambda = 0.5 # tuning parameter 1
beta_start = rep(2, 2) # starting point 2



# Third set of parameters
lambda = 1 # tuning parameter 2
beta_start = rep(0, 2) # starting point 1

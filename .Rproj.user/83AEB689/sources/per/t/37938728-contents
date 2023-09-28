# Create a toy data example
p = 200
n = 50

# Generate matrix of covariates
library(mnormt)
# set.seed(23945)
Sigma = matrix(0.7, p, p) + diag(rep(0.3, p)) # covariance matrix for covariates
X = rmnorm(n, varcov = Sigma)

# Generate response
beta0 = 2 # intercept
sigma = 0.7 # noise standard deviation
beta = runif(p, min = -2, max = 2)
Y = beta0 + X %*% beta + rnorm(n, sd = sigma)


# [ToDo] Split the data into K = 2 folds
# Create idfold vector of length n indicating the fold id, should have 1 or 2 in each position


# [ToDo] Split the data into any K folds, i.e try K = 6. Again, create idfold, should have 1 to K in each position
K = 2
fold_ids <- sample(1:n) %% K + 1

# For loop over folds and lambdas
##########################################
lambda_seq = seq(0.01, 10, length.out = 20)
nlambda = length(lambda_seq)
cvm = rep(NA, nlambda) # want to have CV(lambda)
cvse = rep(NA, nlambda) # want to have SE_CV(lambda)
# Feel free to store additional things
cv_folds = matrix(NA, K, nlambda) # fold-specific information

errors_all = matrix(NA, n, nlambda)

for (fold in 1:K){
  #[ToDo] Create training data xtrain and ytrain, everything except fold
  xtrain = X[fold_ids != fold, ]
  ytrain = Y[fold_ids != fold]
  
  #[ToDo] Create testing data xtest and ytest, everything in fold
  xtest = X[fold_ids == fold, ]
  ytest = Y[fold_ids == fold]
  
  #[ToDo] Center training data
  meanY = mean(ytrain)
  meansX = colMeans(xtrain)
  ytrain = ytrain - meanY # center response
  xtrain = xtrain - matrix(meansX, nrow(xtrain), ncol(xtrain), byrow = T)
  ntrain = nrow(xtrain)

  #[ToDo] For loop over lambdas
  for (i in 1:length(lambda_seq)){
    # [ToDo] Calculate ridge solution
    lambda = lambda_seq[i]
    beta_ridge = solve(crossprod(xtrain)/ntrain + lambda * diag(p), crossprod(xtrain, ytrain)/ntrain)
    
    # [ToDo] Complete with anything else you need for cvm and cvse
    beta0 = meanY - sum(meansX * beta_ridge)
    
    errors_all[fold_ids == fold, i] = (ytest - beta0 - xtest %*% beta_ridge)^2 #ntest by 1
  }
}
# In the end, want to have cvm and cvse
cvm = colMeans(errors_all)

# Feel free to store additional things
cv_folds = matrix(NA, K, nlambda) # fold-specific information

for (fold in 1:K){
  cv_folds[fold, ] = colMeans(errors_all[fold_ids == fold, ])
}

cvse = apply(cv_folds, 2, function(x) sd(x)/sqrt(K))


plot(lambda_seq, cvm)

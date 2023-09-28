# Soft-thresholding function of scalar x at level lambda, returns S(x, lambda)
softthresh <- function(x, lambda){
  # [ToDo] Fill in to return S(x, lambda)
  if (x > lambda){
    # Positive large, moved towards zero
    return(x - lambda)
  }else if (x < -lambda){
    # Negative large, moved towards zero
    return(x + lambda)
  }else{
    # |x| < lambda
    return(0)
  }
}


# Lasso objective function given
# X - n x p matrix of covariates
# Y - n-dimensional response vector
# beta - p-dimensional current value of beta (can assume p=2 for now)
# lambda - non-negative scalar
lassoobj <- function(X, Y, beta, lambda){
  # [ToDo] Fill in to return f(beta) = (2n)^{-1}\|Y-X\beta\|_2^2 + \lambda \|beta\|_1
  # Get sample size
  n = length(Y)
  # Calculate objective
  fobj = sum((Y - X %*% beta)^2) / (2 * n) + lambda * abs(beta[1]) + lambda * abs(beta[2])
  # Return
  return(fobj)
}


# Coordinate descent for LASSO (X already scaled, no intercept, p = 2 case)
coordinateLasso <- function(X, Y, beta_start, lambda, niter = 50){
  
  # Store objective values across iterations
  fobj_vec = c(lassoobj(X, Y, beta = beta_start, lambda), rep(0, niter))
  
  # Get sample size
  n = length(Y)
  
  # Current beta is the starting point
  beta = beta_start
  
  # [ToDo] Fill in the iterations for coordinate-descent
  for (i in 2:(niter + 1)){
    # [ToDo] Update of beta1
    a = crossprod(X[ , 1], Y - X[ , 2] * beta[2])/n
    beta[1] = softthresh(a, lambda)
    
    # [ToDo] Update of beta2
    a = crossprod(X[ , 2], Y - X[ , 1] * beta[1])/n
    beta[2] = softthresh(a, lambda)   
    
    # [ToDo] Calculate updated value of f(beta)
    fobj_vec[i] = lassoobj(X, Y, beta, lambda)
  }
  
  # Return final beta after niter iterations, and the whole vector of objective values
  return(list(beta = beta, fobj_vec = fobj_vec))
}

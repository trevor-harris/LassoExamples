# Soft-thresholding function of scalar x at level lambda, returns S(x, lambda)
softthresh <- function(x, lambda){
  # [ToDo] Fill in to return S(x, lambda)
  if (x > lambda){
    return(x - lambda)
  }else if (x < -lambda){
    return(x + lambda)
  }else{
    return(0)
  }
}


# Lasso objective function given
# X - n x p matrix of covariates
# Y - n-dimensional response vector
# beta - p-dimensional current value of beta
# lambda - non-negative scalar
lassoobj <- function(X, Y, beta, lambda){
  # [ToDo] Fill in to return f(beta) = (2n)^{-1}\|Y-X\beta\|_f^2 + \lambda \|beta\|_1
  n = length(Y)
  
  (2*n)^{-1} * sum((Y - X %*% beta)^2) + lambda * sum(abs(beta))
  
}

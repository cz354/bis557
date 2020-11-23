#' @title Lasso regression using python when the design matrix is orthogonal
#' @description  This function is for lasso regression using python
#' @param  Y The response of the data
#' @param  X Design matrix
#' @param  lambda The penalty parameter for ridge regression
#' @examples
#' n=1000
#' p=4
#' X <- matrix(rnorm(n*p),n,p)
#' beta_true <- matrix(c(2,4,1.5,5),p,1)
#' Y <- X%*%beta_true
#' lm_lasso_py_or(Y,X,lambda=0.01)$coefficients
#' @import reticulate
#' @export

  lm_lasso_py_or <- function(Y,X, lambda){
  p=ncol(X)
  np <- import("numpy", as = "np", convert = FALSE)
  qr=np$linalg$qr(X)
  beta_LS=np$linalg$inv(qr[[1]])$dot(qr[[0]]$T)$dot(Y)
  beta=np$multiply(np$sign(beta_LS)$T,np$maximum(np$subtract(np$abs(beta_LS)$T,np$'repeat'(lambda,p)),0))
  ret <- list("coefficients" = py_to_r(beta$T))
  return (ret)
  }

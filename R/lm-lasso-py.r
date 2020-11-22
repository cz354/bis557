#' @title LASSO regression using python
#' @description  This function is for lasso regression using python
#' @param  Y The response of the data
#' @param  X Design matrix
#' @param  lambda The penalty parameter for LASSP regression
#' @examples
#' data(iris)
#' Y=matrix(iris$Sepal.Length, ncol = 1)
#' X=model.matrix(Sepal.Length ~ .,iris)
#' lm_lasso_py(Y,X,lambda=0)$coefficients
#' @import reticulate
#' @export

  lm_lasso_py <- function(Y,X, lambda){
  np <- import("numpy", as = "np", convert = FALSE)
  n=nrow(X)
  qr=np$linalg$qr(X)
  beta <- np$multiply(np$sign(qr[[1]]$T$dot(qr[[0]]$T)$dot(Y)),np$linalg$inv(qr[[1]]$T$dot(qr[[1]]))$dot(np$maximum(np$subtract(np$abs(qr[[1]]$T$dot(qr[[0]]$T)$dot(Y)),n*lambda),0)))
  ret <- list("coefficients" = py_to_r(beta))
  return (ret)
  }

#' @title Ridge regression using python
#' @description  This function is for ridge regression using python
#' @param  Y The response of the data
#' @param  X Design matrix
#' @param  lambda The penalty parameter for ridge regression
#' @examples
#' data(iris)
#' Y=matrix(iris$Sepal.Length, ncol = 1)
#' X=model.matrix(Sepal.Length ~ .,iris)
#' lm_ridge_py(Y,X,lambda=0.5)$coefficients
#' @import reticulate
#' @export

  lm_ridge_py <- function(Y,X, lambda){
  np <- import("numpy", as = "np", convert = FALSE)
  svd_x <- r_to_py(np$linalg$svd(X,full_matrices = FALSE))
  #dimension
  svd_x[[0]]$shape
  svd_x[[1]]$shape
  svd_x[[2]]$shape
  Sigma <- np$diag(svd_x[[1]])
  lambda_I <- np$diag(np$'repeat'(lambda,Sigma$shape[0]))
  beta <- svd_x[[2]]$T$dot(np$linalg$inv(np$add(np$power(Sigma,2),lambda_I)))$dot(Sigma)$dot(svd_x[[0]]$T)$dot(Y)

  ret <- list("coefficients" = py_to_r(beta))
  return (ret)
  }

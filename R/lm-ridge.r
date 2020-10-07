#' @title Ridge regression
#' @description  This function is for ridge regression
#' @param  formula a formula of linear model
#' @param  data_frame a data_frame
#' @param  contrasts Default is NULL. a list of constasts for factor variable
#' @param  lambda The penalty parametor for ridge regression
#' @examples 
#' data(iris)
#' lm_ridge(Sepal.Length ~ ., iris,lambda=0.5)$coefficients
#' @export

  lm_ridge <- function(formula, data_frame,contrasts=NULL, lambda){
  data_frame_no_na=model.frame(formula,data_frame)
  x=model.matrix(formula,data_frame,contrasts=contrasts)
  x_name=colnames(x)
  
  y_name=as.character(formula)[2]
  y=matrix(data_frame_no_na[, y_name], ncol = 1)
  
  svd_x <- svd(x)
  
  Sigma <- diag(svd_x$d)
  lambda_I <- diag(rep(lambda, length(svd_x$d)))
  beta <- svd_x$v %*% solve(Sigma^2 + lambda_I,tol = 1e-18) %*% Sigma %*% t(svd_x$u) %*% y
  ret <- list(form = formula, coefficients = beta)
  class(ret) <- "ridge"
  ret
  }
  

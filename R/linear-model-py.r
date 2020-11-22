#' @title fit the linear model using python
#' @description This a simple function to fit the linear model using python.
#' @param  Y The response variable
#' @param  X Design matrix
#' @examples
#' data(iris)
#' Y=matrix(iris$Sepal.Length, ncol = 1)
#' X=model.matrix(Sepal.Length ~ .,iris)
#' linear_model_py(Y,X)$coefficients
#' 
#' @import reticulate
#' @export
linear_model_py<-function(Y,X){

  np <- import("numpy", as = "np", convert = FALSE)
  qr=np$linalg$qr(X)
  beta=np$linalg$inv(qr[[1]])$dot(qr[[0]]$T)$dot(Y)
  list("coefficients"=py_to_r(beta))

}

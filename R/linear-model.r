#' @title fit the linear model 
#' @description This a simple function to fit the linear model.
#' @param  formula a formula of linear model
#' @param  data_frame a data_frame
#' @param  contrasts default is NULL. a list of constasts for factor variables
#' @examples
#' data(iris)
#' linear_model(Sepal.Length ~ ., iris,contrasts = NULL)$coefficients
#' @export
linear_model<-function(formula, data_frame,contrasts = NULL){
  
  data_frame_no_na=model.frame(formula,data_frame)
  
  x=model.matrix(formula,data_frame,contrasts=contrasts)
  
  y_name=as.character(formula)[2]
  y=matrix(data_frame_no_na[, y_name], ncol = 1)
  
  beta=qr.coef(qr(x),y)
  
  list(coefficients=beta)
  
}

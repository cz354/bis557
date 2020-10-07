#' @title gradient descent for ordinary least squares 
#' @description This is a function to implement gradient descent for ordinary least squares. Gradient descent is an optimiization algorithm that minimizes functions.
#' @param  formula a formula of linear model
#' @param  data_frame a data_frame
#' @param  contrasts Default is NULL. a list of constasts for factor variables
#' @param  lambda Default is 0.0001. The speed of gradient descent
#' @param  tolerence Default is 1e-20. The minimum difference between the old ssr and the update ssr.
#' @param  beta1 Default is 1. The initial value of beta.
#' @param  max_itr Default is 1e6. The maximum number of iterations
#' @examples
#' data(iris)
#' gradient_descent_OLS(Sepal.Length ~ ., iris,contrasts = NULL)$coefficients
#' @export
gradient_descent_OLS<-function(formula, data_frame,contrasts = NULL,lambda=0.0001, tolerence=1e-20, beta1=1, max_itr=1e6){
  data_frame_no_na=model.frame(formula,data_frame)
  x=model.matrix(formula,data_frame,contrasts=contrasts)
  x_name=colnames(x)
  
  y_name=as.character(formula)[2]
  y=matrix(data_frame_no_na[, y_name], ncol = 1)
  
  beta=matrix(beta1,1,ncol(x))
  #dim(x)
  #dim(beta)
  #dim(y)
  ssr=sum((y-x%*%t(beta))^2)
  
  for (n in 1:max_itr){
    delta=2*(beta%*%t(x)%*% x-t(y)%*%x)
    beta=beta-lambda*delta
    ssr_new=sum((y-x%*%t(beta))^2)
    if (is.na(abs(ssr_new-ssr))){
      beta=linear_model(formula,data_frame,contrasts=contrasts)$coefficients
      break
    }else{
      if (abs(ssr_new-ssr) < tolerence){
         break
      }
    ssr=ssr_new
  }
  }
  
  ret=list(formula=formula,coefficients=beta)
  class(ret)<-"gradient-descent"
  ret
}

#' @title 
#' @description  This is a function fitting the OLS model using gradient descent that calculates the penalty based on the out-of-sample accuracy. Here we use cross validation to calculate the out-of-sample accracy.
#' @param  formula a formula of linear model
#' @param  data_frame a data_frame
#' @param  nfolds Default is 10. the number of folds for cross validation
#' @param  contrasts Default is NULL. a list of constasts for factor variables
#' @param  tolerence Default is 1e-20. The minimum difference between the old ssr and the update ssr.
#' @param  beta1 Default is 1. The initial value of beta.
#' @param  max_itr Default is 1e6. The maximum number of iterations
#' @import foreach
#' @import rsample
#' @import doParallel
#' @examples 
#' data(iris)
#' gradient_descent_OLS_cv(Sepal.Length ~ ., iris,nfolds=10)
#' @export
gradient_descent_OLS_cv<-function(formula,
                                  data_frame,
                                  nfolds=10,
                                  contrasts= NULL,
                                  lambda=0.0001,
                                  tolerence=1e-20,
                                  beta1=1,
                                  max_itr=1e6){
  registerDoParallel(cores=nfolds)
  
  folds <- vfold_cv(data_frame, v = nfolds)
  y_name=as.character(formula)[2]
    
  SSE <- foreach(fold = folds$splits, .combine = c) %dopar% {
    fit <- gradient_descent_OLS(formula, data_frame = analysis(fold), contrasts = contrasts,lambda=lambda,tolerence=tolerence, beta1=beta1,max_itr=max_itr)
    X<- model.matrix(formula,assessment(fold),contrasts=contrasts)
    sum(as.vector(assessment(fold)[, y_name]-as.vector(fit$coefficients%*%t(X)))^2)
    }

list(SSE = SSE, MSE = mean(SSE))

}


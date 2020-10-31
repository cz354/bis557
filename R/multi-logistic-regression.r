#' @title a classification model genralizing logistic regression to accommodate more than two classes.
#' @description a classification model genralizing logistic regression to accommodate more than two classes.
#'
#' @param X data matrix
#' @param Y Response vector
#' @param  tol Default is 1e-20. The minimum difference between the old beta and the update beta.
#' @param  maxit Default is 10000. The maximum number of iterations.
#'
#' @examples
#' data("iris")
#' X = iris[,-5]
#' Y=iris$Species
#' multi_logistic_regression(X,Y)
#' @export


 multi_logistic_regression <- function(X,Y,maxit =10000 ,tol =1e-20){
   
   K= length(unique(Y))
   X= as.matrix(cbind(1,X))
   beta= matrix(0,ncol(X),K)
   prob= matrix(0,nrow(X),K)
   for(k in (1:K)){
     beta_diff<-c()
     y=ifelse(Y==levels(Y)[k],1,0)
     
     for (i in seq_len(maxit)){
       beta_old <- matrix(beta[,k],5,1)
       p <- 1/(1+exp(-X%*% beta_old))
       D <- diag(as.vector(p))
       XtDX <- t(X) %*% D %*% X
       inv_hessian <- -solve(XtDX)
       grad <- t(X)%*%(y-matrix(p,ncol =1))
       beta[,k] <- beta_old -inv_hessian %*% grad
       prob[,k] <- p
       beta_diff <- c(beta_diff,sum((beta[,k]-beta_old)^2))
       if( tail(beta_diff,1)<= tol){
        break
      }
     }
     
   }
   category=apply(prob, 1, which.max)
   y_pred=as.factor(levels(Y)[category])
   list(y_pred=y_pred)
  }



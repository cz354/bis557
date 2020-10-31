#' @title a first-order solution for the GLM maximum likelihood problem using only gradient information.
#' @description This function solves GLM with a first order solution, which avoids the Hessian matrix. Here we could use either a constant step size or a an adaptive step size.
#'
#' @param X data matrix
#' @param Y Response vector
#' @param mu_fun The inverse of link function
#' @param steps the step size.
#' @param  tol Default is 1e-20. The minimum difference between the old beta and the update beta.
#' @param  maxit Default is 10000. The maximum number of iterations.
#'
#' @examples
#'set.seed(100)
#'maxit=10000
#'steps1=rep(0.001,maxit)
#'n=100 
#'p=2
#'X=matrix(rnorm(n*p),n,p)
#'X=cbind(1,X)
#'beta=matrix(c(2,4,6),1,3)
#'prob=exp(beta%*%t(X))/(1+exp(beta%*%t(X)))
#'y=rbinom(n,size = 1, prob = prob)
#'glm_first_order(X,y, binomial(link="logit")$linkinv,steps1,maxit=maxit)
#' @export

glm_first_order <- function(X, Y, mu_fun, steps,maxit=10000, tol= 1e-20){
  
  beta <- matrix(rep(0, ncol(X),ncol=1))
  beta_diff <- c()
  for(i in seq_len(maxit)){
    beta_old <- beta
    eta <- X%*%beta_old
    mu <- mu_fun(eta)
    grad <- t(X)%*%(Y-mu)
    beta <- beta_old +steps[i]*grad
    beta_diff <- c(beta_diff,abs(sum((beta-beta_old))))
    if( tail(beta_diff,1)<= tol){
        break
      }
  }
  list(beta= beta)
}


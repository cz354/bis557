library(testthat)
  
context("Test the output of homework 3.")

test_that("You glm_first_order() function works in constant step sizes", {
set.seed(100)
maxit=10000
steps1=rep(0.001,maxit)
n=100 
p=2
X=matrix(rnorm(n*p),n,p)
X=cbind(1,X)
beta=matrix(c(2,4,6),1,3)
prob=exp(beta%*%t(X))/(1+exp(beta%*%t(X)))
y=rbinom(n,size = 1, prob = prob)
fit=glm_first_order(X,y, binomial(link="logit")$linkinv,steps1,maxit=maxit)



expect_equivalent(t(beta), fit$beta,
                    tolerance = 0.5)
})

context("Test the output of homework 3.")

test_that("You glm_first_order() function works in additive step sizes", {

set.seed(100)
maxit=10000
steps2=seq(5e-3,5e-7,length=maxit)
n=100 
p=2
X=matrix(rnorm(n*p),n,p)
X=cbind(1,X)
beta=matrix(c(2,4,6),1,3)
prob=exp(beta%*%t(X))/(1+exp(beta%*%t(X)))
y=rbinom(n,size = 1, prob = prob)
fit=glm_first_order(X,y, binomial(link="logit")$linkinv,steps2,maxit=maxit)



expect_equivalent(t(beta), fit$beta,
                    tolerance = 0.5)
})

library(testthat)
  
context("Test the output of homework 4.")

test_that("You lm_lasso_py_or() function works in an easy case.", {

  n=1000
  p=4
  X <- matrix(rnorm(n*p),n,p)
  beta_true <- matrix(c(2,4,1.5,5),p,1)
  Y <- X%*%beta_true
  fit_lasso <- lm_lasso_py_or(Y,X,lambda=0.01)

  fit_casl= casl::casl_lenet(X,Y,lambda=0.01,maxit=1000L)
  expect_equivalent(fit_casl, fit_lasso$coefficients,
                    tolerance = 1e-2)
})




library(testthat)
  
context("Test the output of homework 4.")

test_that("You lm_lasso_py() function works in an easy case.", {

  data(iris)
  Y=matrix(iris$Sepal.Length, ncol = 1)
  X=model.matrix(Sepal.Length ~ .,iris)

  fit_lasso <- lm_lasso_py(Y,X,lambda=0)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, fit_lasso$coefficients,
                    tolerance = 1e-5)
})




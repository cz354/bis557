library(testthat)
  
context("Test the output of homework 4.")

test_that("You lm_ridge_py() function works in an easy case.", {

  data(iris)
  Y=matrix(iris$Sepal.Length, ncol = 1)
  X=model.matrix(Sepal.Length ~ .,iris)

  fit_ridge <- lm_ridge_py(Y,X,lambda=0.5)

  fit_lm <- lm_ridge(Sepal.Length  ~ ., iris,lambda=0.5)

  expect_equivalent(fit_lm$coefficients, fit_ridge$coefficients,
                    tolerance = 1e-5)
})




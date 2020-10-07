library(testthat)

context("Test the output of homework 2.")

test_that("You lm_ridge() function works in an easy case.", {
  
  data(iris)
  
  fit_ridge <- lm_ridge(Sepal.Length ~ ., iris,lambda=0)
  
  fit_lm <- lm(Sepal.Length  ~ ., iris)
  
  expect_equivalent(fit_lm$coefficients, fit_ridge$coefficients,
                    tolerance = 1e-5)
})

test_that("You lm_ridge() function works with contrasts.", {
  
  data(iris)
  
  fit_ridge <- lm_ridge(Sepal.Length ~ ., iris,lambda=0,
                                   contrasts = list(Species = "contr.sum"))
  
  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))
  
  expect_equivalent(fit_lm$coefficients, fit_ridge$coefficients,
                    tolerance = 1e-5)

})

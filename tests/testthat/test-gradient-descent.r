library(testthat)

context("Test the output of homework 1.")

test_that("You gradient_descent_OLS() function works in an easy case.", {

  data(iris)

  fit_linear_model <- gradient_descent_OLS(Sepal.Length ~ ., iris)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
                    tolerance = 1e-4)
})

test_that("You gradient_descent_OLS() function works with contrasts.", {

  data(iris)

  fit_linear_model <- gradient_descent_OLS(Sepal.Length ~ ., iris, 
                                   contrasts = list(Species = "contr.sum"))

  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))

  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
                    tolerance = 1e-5)
})



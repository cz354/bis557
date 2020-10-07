context("Test the output of homework 2.")

test_that("You gradient_descent_OLS_cv() function works in an easy case.", {
  
  data(iris)
  
  fit_gradient <- gradient_descent_OLS_cv(Sepal.Length ~ ., iris)
  
  folds <- vfold_cv(iris, v = 10)
  formula <- Sepal.Length ~ .
  SSE <- foreach(fold = folds$splits, .combine = c) %do% {
    fit <- lm(formula=formula, data = analysis(fold))
    sum(as.vector(assessment(fold)[, as.character(formula)[2]] -
                    as.vector(predict(fit, assessment(fold))))^2)
  }  
  
  expect_equivalent(mean(SSE), fit_gradient$MSE,
                    tolerance = 0.1)
})

test_that("You gradient_descent_OLS_cv() function works with contrasts.", {
  
  data(iris)
  
  fit_gradient <- gradient_descent_OLS_cv(Sepal.Length ~ ., iris,
                                           contrasts = list(Species = "contr.sum"))
  
  
  folds <- vfold_cv(iris, v = 10)
  formula <- Sepal.Length ~ .
  SSE <- foreach(fold = folds$splits, .combine = c) %do% {
    fit <- lm(formula=formula, data = analysis(fold),contrasts =list(Species = "contr.sum"))
    sum(as.vector(assessment(fold)[, as.character(formula)[2]] -
                    as.vector(predict(fit, assessment(fold))))^2)
  }
  expect_equivalent(mean(SSE), fit_gradient$MSE,
                    tolerance = 0.1)
})







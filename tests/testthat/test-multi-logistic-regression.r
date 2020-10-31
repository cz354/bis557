library(testthat)

context("Test the output of homework 3.")

test_that("You multi_logistic_regression() function works ", {
data("iris")
X = iris[,-5]
Y=iris$Species
fit=multi_logistic_regression(X,Y)
acc=sum(fit$y_pred==Y)/length(Y)
expect_equivalent(acc,1,tol=0.1)

})

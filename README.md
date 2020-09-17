# bis557

This is the package for homework1 of BIS557 at Yale university

There are two functions on the package: linear_model() and gradient_descent_OLS()

There is also a dataset lm_patho in this package.

## Example
```{r}
data(iris)

#The example for function linear_model
linear_model(Sepal.Length ~ ., iris,contrasts = NULL)$coefficients

#The example for function gradient_descent_OLS
gradient_descent_OLS(Sepal.Length ~ ., iris,contrasts = NULL)$coefficients
```

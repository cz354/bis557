
# bis557

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/cz354/bis557.svg?branch=master)](https://travis-ci.org/cz354/bis557)
[![codecov](https://codecov.io/gh/cz354/bis557/branch/master/graph/badge.svg)](https://codecov.io/gh/cz354/bis557)
[![Travis build status](https://travis-ci.com/cz354/bis557.svg?branch=master)](https://travis-ci.com/cz354/bis557)
[![Coveralls test coverage](https://coveralls.io/repos/github/cz354/bis557/badge.svg)](https://coveralls.io/r/cz354/bis557?branch=master)
<!-- badges: end -->

The goal of bis557 is to include the material of the homework1 for the Yale Biostatistics bis557 class.

## Installation

The development version of the packages can be installed from [GitHub](https://github.com/) with:


``` r
# install.packages("devtools")
devtools::install_github("kaneplusplus/bis557")

install.packages("bis557")
```

## Example

This Example of the function linear_model().

``` r
data(iris)
linear_model(Sepal.Length ~ ., iris,contrasts = NULL)$coefficients
```
This Example of the function gradient_descent_OLS().

``` r
data(iris)
gradient_descent_OLS(Sepal.Length ~ ., iris,contrasts = NULL)$coefficients
```

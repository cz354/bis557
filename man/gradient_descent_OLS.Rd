% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gradient-descent.r
\name{gradient_descent_OLS}
\alias{gradient_descent_OLS}
\title{gradient descent for ordinary least squares}
\usage{
gradient_descent_OLS(
  formula,
  data_frame,
  contrasts = NULL,
  lambda = 1e-04,
  tolerence = 1e-20,
  beta1 = 1,
  max_itr = 1e+06
)
}
\arguments{
\item{formula}{a formula of linear model}

\item{data_frame}{a data_frame}

\item{contrasts}{Default is NULL. a list of constasts for factor variables}

\item{lambda}{Default is 0.0001. The speed of gradient descent}

\item{tolerence}{Default is 1e-20. The minimum difference between the old ssr and the update ssr.}

\item{beta1}{Default is 1. The initial value of beta.}

\item{max_itr}{Default is 1e6. The maximum number of iterations}
}
\description{
This is a function to implement gradient descent for ordinary least squares. Gradient descent is an optimiization algorithm that minimizes functions.
}
\examples{
data(iris)
gradient_descent_OLS(Sepal.Length ~ ., iris,contrasts = NULL)$coefficients
}

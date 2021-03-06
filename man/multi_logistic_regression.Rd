% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi-logistic-regression.r
\name{multi_logistic_regression}
\alias{multi_logistic_regression}
\title{a classification model genralizing logistic regression to accommodate more than two classes.}
\usage{
multi_logistic_regression(X, Y, maxit = 10000, tol = 1e-20)
}
\arguments{
\item{X}{data matrix}

\item{Y}{Response vector}

\item{maxit}{Default is 10000. The maximum number of iterations.}

\item{tol}{Default is 1e-20. The minimum difference between the old beta and the update beta.}
}
\description{
a classification model genralizing logistic regression to accommodate more than two classes.
}
\examples{
data("iris")
X = iris[,-5]
Y=iris$Species
multi_logistic_regression(X,Y)
}

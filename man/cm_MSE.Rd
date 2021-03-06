% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cm_MSE.R
\name{cm_MSE}
\alias{cm_MSE}
\title{Mean squared error}
\usage{
cm_MSE(y, yhat)
}
\arguments{
\item{y}{Response variable observations}

\item{yhat}{Response variable predictions}
}
\value{
The mean squared error of prediction
}
\description{
This function computes in a very simple way the mean squared error from a vector of observations and a vector of predictions
}
\examples{
set.seed(8675309)
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)
y = 1 + .5*x1 + .2*x2 + rnorm(n)
X=cbind(x1,x2)
yhat <- cm_predict()
cm_MSE(y, yhat)
}

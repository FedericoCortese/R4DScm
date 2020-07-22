#' Sequential Leave one out cross validation
#'
#' This function performs a simple LOOCV
#'
#' @param b: vector of initial parameters
#' @param X Covariates Matrix: each column contains observations for each covariate.
#' @param y Response variable observations
#' @param tol Tolerance level for the optimization process, the default is 0.001.
#' @param maxit Maximum iterations number
#'
#' @return The mean squared error of prediction
#'
#' @examples
#' set.seed(8675309)
#' n = 1000
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' y = 1 + .5*x1 + .2*x2 + rnorm(n)
#' X=cbind(x1,x2)

#' b_pre <- c(0,0,0)
#' cm_LOOCV(y, X, b_pre = c(0,0,0))
#' @export

cm_LOOCV <- function(y, X, b_pre, tol = 1e-3, maxit = 1000) {
  n <- length(y)
  err <- vector("double", n)
  for (i in 1:n) {
    y_loocv <- y[-i]
    X_loocv <- X[-i,]
    X_out <- c(1,X[i,])

    beta_loocv <- basic_sd(y = y_loocv, beta = b_pre, X = X_loocv, tol = tol, maxit = maxit)
    yhat_loocv <- X_out%*%beta_loocv$param
    err[i] <- (y[i] - yhat_loocv)
  }
  MSE <- sum(err^2)/n
  return(MSE)
}

#' Parallel Leave one out cross validation
#'
#' This function performs a parallelized LOOCV to evaluate the prediction accuracy of a linear regression whose parameter are computed through a Steepest descent algorithm.
#' It always uses the maximum number of cores possible.

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
#' cm_pllLOOCV(y, X, b_pre = c(0,0,0))
#' @export

cm_pllLOOCV <- function(y, X, b_pre, tol = 1e-3, maxit = 1000) {
  cluster <-  parallel::makeCluster(parallel::detectCores(), type = "SOCK")
  doSNOW::registerDoSNOW(cluster)

  n <- length(y)
  r <- foreach::foreach(i = 1:n, .combine = '+', .export = "basic_sd") %dopar% {
    y_loocv <- y[-i]
    X_loocv <- X[-i,]
    X_out <- c(1,X[i,])

    beta_loocv <- basic_sd(y = y_loocv, X = X_loocv, beta = b_pre, tol = tol, maxit = maxit)
    yhat_loocv <- X_out%*%beta_loocv$param
    err <- (y[i] - yhat_loocv)^2
  }
  parallel::stopCluster(cluster)
  return(r/n)
}

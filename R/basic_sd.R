#' Linear Steepest Descend
#'
#' This function  computes the vector of parameters in a linear regression model via the Steepest Descend Method.
#'
#' @param beta vector of initial parameters
#' @param X Covariates Matrix: each column contains observations for each covariate.
#' @param y Response variable observations
#' @param tol Tolerance level for the optimization process, the default is 0.001.
#' @param maxit Maximum iterations number
#'
#' @return A list containing the fitted values for the beta vector and  the number of iterations performed
#'
#' @examples
#' set.seed(8675309)
#' n = 1000
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' y = 1 + .5*x1 + .2*x2 + rnorm(n)
#' X=cbind(x1,x2)

#' b_pre <- c(0,0,0)
#' basic_sd(y, X, b_pre)
#' @export

basic_sd <- function(y, X, beta, tol = 1e-3, maxit = 1000) {
  X <- cbind(1, X)
  tX <- t(X)
  prod <- tX%*%X
  hessian <- 4*prod

  diff <- tol + 1
  iter <- 0

  while (diff > tol & iter <= maxit) {
    gradient <- 2*(prod%*%beta - tX%*%y)
    stp <- as.numeric((t(gradient)%*%gradient)/(t(gradient)%*%hessian%*%gradient))
    beta.old <- beta
    beta <- beta - stp*gradient

    diff <- max(abs(beta-beta.old))
    iter <- iter + 1
  }
  return(list(param = beta, iter = iter))

}



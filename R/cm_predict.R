#' Predict
#'
#' This function computes the prediction
#'
#' @param X matrix of covariates observations
#' @param beta vector of parameters of a linear regression model
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
#' beta <- basic_sd(y, X, b_pre)
#' cm_predict(X, beta)
#' cm_LOOCV(y, X, b_pre = c(0,0,0))
#' @export

cm_predict <- function(X, beta) {
  yhat <- cbind(1,X)%*%beta
  return(yhat)
}

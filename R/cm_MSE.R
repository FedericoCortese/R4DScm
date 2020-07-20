#' Mean squared error
#'
#' This function computes in a very simple way the mean squared error from a vector of observations and a vector of predictions
#'
#' @param y Response variable observations
#' @param yhat Response variable predictions
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

#' yhat <- cm_predict()
#' cm_MSE(y, yhat)
#' @export


cm_MSE <- function(y, yhat) {
  sum((y-yhat))^2/length(y)
}

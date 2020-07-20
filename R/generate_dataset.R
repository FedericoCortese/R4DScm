#' Generate dataset for examples
#'
#' This function generates the dataset used for examples
#'
#' @param n: number of observations in the dataset
#'
#' @return A list containing the vector of observations for the dependent variable and a matrix containing the observations of the independent variables
#'
#' @examples
#' set.seed(8675309)
#' generate_dataset(n)
#' @export

generate_dataset <- function(n) {
  set.seed(8675309)
  x1 = rnorm(n)
  x2 = rnorm(n)
  X <- cbind(x1, x2)
  y = 1 + .5*x1 + .2*x2 + rnorm(n)
  return(list(y, X))
}

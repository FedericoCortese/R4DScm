#' Linear Gradient Descend (2)
#'
#' This function is the slower version of \code{\link{linear_gd_optim}} given that it makes use of the \code{\link{grad}} function contained in the package "numDeriv".
#'
#' @param b_pre vector of initial parameters
#' @param X Covariates Matrix: each column contains observations for each covariate.
#' @param y Response variable observations
#' @param tolerance Tolerance level for the optimization process, the default is 0.001.
#' @param maxit Maximum iterations number. Default is 1000.
#' @param stepsize The value for the stepsize in the equation of the gradient descend. Default is 0.001.
#'
#' @return A list containing the fitted values for the beta vector and the number of iterations performed.
#'
#' @examples
#' set.seed(8675309)
#' n = 1000
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' y = 1 + .5*x1 + .2*x2 + rnorm(n)
#' X=cbind(x1,x2)

#' b_pre=c(0,0,0)
#' linear_gd_optim(b_pre,X,y)#'
#' @export
linear_gd_optim2 <- function(b_pre,             # beta(0)
                              X,               # data predictors
                              y,               # response variable
                              tolerance=1e-3,  # tolerance
                              maxit=1000,      # max iteration, not to run forever
                              stepsize=1e-3#,   # stepsize parameter
                              #verbose=F
) {
  library(numDeriv)
  L=function(b,X,y){
    return(mean((X%*%b-y)^2))
  }

  X=cbind(1,X)

  b_post=b_pre-grad(L,b_pre,X=X,y=y)*stepsize
  it=1


  while(max(abs(b_pre-b_post))>tolerance){

    if(it==maxit){
      return(b_post)
    }

    else{
      b_pre=b_post
      b_post=b_pre-grad(L,b_pre,X=X,y=y)*stepsize
      it=it+1
    }

  }

  return(list(beta=b_post,iterations=it))

}

#Roxygen2:
#1) scrivi i commenti sopra la function con #'@...
#2) esegui i seguenti:
#   roxygen2::roxygenise()
#   devtools::document()
#   Ctrl + Shift + D

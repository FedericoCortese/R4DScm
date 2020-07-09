#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{linear_gd_optim2}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin parameters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
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

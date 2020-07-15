#' Linear Gradient Descend
#'
#' This function  computes the vector of parameters in a linear regression model via the Gradient Descend Method.
#'
#' @param b_pre vector of initial parameters
#' @param X Covariates Matrix: each column contains observations for each covariate.
#' @param y Response variable observations
#' @param tolerance Tolerance level for the optimization process, the default is 0.001.
#' @param maxit Maximum iterations number
#' @param stepsize The value for the stepsize in the equation of the gradient descend
#'
#' @return A list containing the fitted values for the beta vector and the number of iterations performed
#'
#' @examples
#' set.seed(8675309)
#' n = 1000
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' y = 1 + .5*x1 + .2*x2 + rnorm(n)
#' X=cbind(x1,x2)

#' b_pre=c(0,0,0)
#' linear_gd_optim(b_pre,X,y)
#' @export
linear_gd_optim <- function(b_pre,             # beta(0)
                            X,               # data predictors
                            y,               # response variable
                            tolerance=1e-3,  # tolerance
                            maxit=1000,      # max iteration, not to run forever
                            stepsize=1e-3#,   # stepsize parameter
                            #verbose=F
) {

  gradL=function(b,X,y){

    l=numeric()
    k=length(b)

    for(i in 1:k){
      l[i]=2*mean((X%*%b-y)*X[,i])
    }
    return(l)
  }
  X=cbind(1,X)

  b_post=b_pre-gradL(b_pre,X=X,y=y)*stepsize
  it=1

  while(max(abs(b_pre-b_post))>tolerance){

    if(it==maxit){
      return(b_post)
    }

    else{
      b_pre=b_post
      b_post=b_post-gradL(b_post,X,y)*stepsize
      it=it+1
    }

  }

  return(list(beta=b_post,iterations=it))

}



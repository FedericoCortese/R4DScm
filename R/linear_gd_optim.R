#' Linear Gradient Descend
#'
#' Creates a plot of the crayon colors in \code{\link{linear_gd_optim}}
#'
#' @param method2order method to order colors
#' @param cex character expansion for the text
#' @param mar margin parameters; vector of length 4
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
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



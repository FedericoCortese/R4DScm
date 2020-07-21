test_that("basic_sd works", {

  set.seed(8675309)
  n = 1000
  x1 = rnorm(n)
  x2 = rnorm(n)
  y = 1 + .5*x1 + .2*x2 + rnorm(n)
  X=cbind(x1,x2)
  maxit=10000
  tolerance=1e-6
  stepsize=1e-3

  b_pre=c(0,0,0)
  estimate=as.vector(basic_sd(y,X,b_pre,tol=tolerance,maxit=maxit)$param)

  expect_equal(round(estimate,3),
               c(1,.5,.2),tolerance=1e-1)

})

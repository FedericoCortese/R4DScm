test_that("basic_sd works", {

  set.seed(8675309)
  n = 1000
  x1 = rnorm(n)
  x2 = rnorm(n)
  y = 1 + .5*x1 + .2*x2 + rnorm(n)
  X=cbind(x1,x2)
  maxit=10000
  tolerance=1e-6

  b_pre=c(0,0,0)
  estimate=as.vector(basic_sd(b_pre,X,y,tol=tolerance,maxit=maxit)$param)

  fit=as.vector(coef(lm(y~x1+x2)))

  expect_equal(estimate,
               fit,tolerance=1e-4)

})

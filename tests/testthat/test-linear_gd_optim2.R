test_that("linear_grad_optim2 works", {

  set.seed(8675309)
  n = 1000
  x1 = rnorm(n)
  x2 = rnorm(n)
  y = 1 + .5*x1 + .2*x2 + rnorm(n)
  X=cbind(x1,x2)
  maxit=10000
  tol=1e-6
  stepsize=1e-3

  b_pre=c(0,0,0)
  estimate=as.vector(linear_gd_optim2(b_pre,X,y,tol=tol,maxit=maxit,stepsize = stepsize)$param)

  fit=as.vector(coef(lm(y~x1+x2)))

  expect_equal(estimate,
               fit,tolerance=1e-4)

})

test_that("linear_grad_optim works", {

  set.seed(8675309)
  n = 1000
  x1 = rnorm(n)
  x2 = rnorm(n)
  y = 1 + .5*x1 + .2*x2 + rnorm(n)
  X=cbind(x1,x2)

  b_pre=c(0,0,0)
 # fit=linear_gd_optim(b_pre,X,y)

  expect_equal(linear_gd_optim(b_pre,X,y,maxit = 10000,tolerance = 1e-6)$beta,
               c(1,.5,.2),tolerance=1e-1)

})

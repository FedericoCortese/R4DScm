library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(ggplot2)
library(tidyr)


show_bm=function(bm){
  print(bm)
  autoplot(bm)
}

set.seed(8675309)
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)
y = 1 + .5*x1 + .2*x2 + rnorm(n)
X=cbind(x1,x2)

b_pre=c(0,0,0)

bench::mark(
  check=F,
  bare=NULL,
  first=linear_gd_optim(b_pre,X,y),
  second=linear_gd_optim2(b_pre,X,y)
) %>% show_bm()
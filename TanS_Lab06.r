
f <- function(x){
  x^2
}

fprime<-function(x){
  2*x
}

deriv <-function(x,h){
  (f(x+h)-f(x))/h
}

err <- function(h){
  abs(fprime(1)-deriv(1,h))
}
h <- seq(0.000001,1,len=100)
plot(h,err(h),type="l")
# Name: Siyun Tan
# Course: 44-149 Scientific Computing
# Lab07:(get help from gloria zhang)
# Due Date: 16/3/2018
# Brief: 
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy


MyData <- read.csv(file="points.csv", header=TRUE, row.name=1)
m <- as.matrix(MyData)

if (!require(plotly)){
  install.packages("plotly")
  require(plotly)
}


translate <- function(x,y,z){
  matrix(c(1,0,0,x,0,1,0,y,0,0,1,z,0,0,0,1),nrow=4, ncol=4, byrow=TRUE)
}

scale <- function(x,y,z){
  matrix(c(x,0,0,0,0,y,0,0,0,0,z,0,0,0,0,1),nrow=4, ncol=4, byrow=TRUE)
}

project <- function(l,r,b,t,n,f){
  matrix(c(2 * n/(r-1),0,(r+1)/(r-1),0,0,2 * n/(t-b),(t+b)/(t-b),0,0,0,-(f+n)/(f-n),-2*f*n/(f-n),0,0,-1,0),nrow=4, ncol=4, byrow=TRUE)
}

s <- scale(2,2,2)
s0 <- scale(0.5,0.5,2)
m0 <- S %*% m
T0 <- translate(0,0,-4)
s <- scale(1.5,1.5,1.5)
T01 <-translate(0,0,4)
m01 <- T01 %*% S %*% T0%*%m
m02 <- project(-2,2,-2,2,2,20)%*%m
m02[3,] <- 0
T3 <- translate(3,3,-1)
m03 <- s0 %*% m
m04 <- T01 %*% S %*% T0%*% m
m05 <- T3 %*% m



allm <- cbind(m, m0,m01,m02,m03,m04,m05) 
labels <- c(rep('original', ncol(m)), 
            rep('scaled', ncol(m)),
            rep('original1',ncol(m)),
            rep('scaled2',ncol(m)),
            rep('scaled2', ncol(m)),
            rep('scaled2',ncol(m)),
            rep('scaled3',ncol(m)))
p <- plot_ly( x=allm[1,] / allm[4,], y=allm[2,] / allm[4,], z=allm[3,] / allm[4,], type='scatter3d', mode='lines+markers', color=labels)
print(p)
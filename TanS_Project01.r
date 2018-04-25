# Name: Siyun Tan
# Course: 44-149 Scientific Computing
# Assignment project 1
# Due Date: 16/3/2018
# Brief: Got Help from Rebecca 
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy

SIGMA <- 10
RHO <- 28
BETA <- 8.0/3
TMAX <- 100
ITERS <- 10000
X0 <- 1
Y0 <- 1
Z0 <- 1


dx <- function(x, y, z, dt){
  SIGMA * (y-x) * dt
}
dy <- function(x, y, z, dt){
  (x * (RHO - z) - y)*dt
}
dz <- function(x, y, z, dt){
  (x * y - BETA * z)*dt
}


table <- function (X0, Y0, Z0){
  t <- seq(0, TMAX, length=ITERS)
  x <- rep(0, ITERS)
  y <- rep(0, ITERS)
  z <- rep(0, ITERS)
  color <- rep(0, ITERS)
  x[1] <- X0
  y[1] <- Y0
  z[1] <- Z0
  for (i in 2:ITERS){
    dt <- (t[i]-t[i-1])
    x[i] <- x[i-1] + dx(x[i-1], y[i-1], z[i-1], dt)
    y[i] <- y[i-1] + dy(x[i-1], y[i-1], z[i-1], dt)
    z[i] <- z[i-1] + dz(x[i-1], y[i-1], z[i-1], dt)
  }
  table <- data.frame(x, y, z)
}

if (!require(plotly)){
  install.packages("plotly")
  require(plotly)
}

table1 = table(X0, Y0, Z0)
table2 = table(1.1, 1.1, 1.1)



p <- plot_ly(x=table1[,'x'], y=table1[,'y'], z=table1[,'z'],
             type='scatter3d', mode='lines', color = I('blue'))
p <- add_trace(p, x=table2[,'x'], y=table2[,'y'], z=table2[,'z'],
               type='scatter3d', mode='lines', color = I('purple'))
print(p)

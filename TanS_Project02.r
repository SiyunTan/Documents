# Name: Siyun Tan
# Course: 44-149 Scientific Computing
# Assignment # Project02
# Due Date: April06
# Brief: A brief description of the assignment
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy

battery <- 500
xs <- rep(0,battery)
ys <- rep(0,battery)
N <- 10 
x0 <- N/2
y0 <- N/2
xs[1] <- x0
ys[1] <- y0
room <- matrix(0, ncol=N, nrow=N)
room[x0,y0] <- 1
count <- 0

for(i in 2:battery){
  xs[i] <- xs[i-1]
  ys[i] <- ys[i-1]
  direction <- sample(1:4,1)
  if(direction == 1){
   xs[i] <- xs[(i-1)]+1
  }
  if(direction == 2){
   xs[i] <- xs[(i-1)]-1
  }
  if(direction == 3){
   ys[i] <- ys[(i-1)]+1
  }
  if(direction == 4){
   ys[i] <- ys[(i-1)]-1
  }
  if(xs[i]>N){
    xs[i] <- N
  }
  if(ys[i]>N){
    ys[i] <- N
  }
  if(ys[i]< 1){
    ys[i] <- 1
  }
  if(xs[i]< 1){
    xs[i] <- 1
  }
  room[xs[i],ys[i]]<-1
}

print(room)
print(sum(room)/100)

# Name: Siyun Tan
# Course: 44-149 Scientific Computing
# Assignment # project02
# Due Date: April06
# Brief: A brief description of the assignment
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy

unitcost <- rnorm(1, mean=6.50, sd=.75)
fixedcost <- 120000
vsales <- 0
pselling <- 0

mode <- sample(1:3,1)

market<- function(mode){
  if(i==1){
    vsales<-50000
    pselling<-11.00
  }else if(i==2){
    vsales<-75000.00
    pselling<-10.00
  }else if(i==3){
    vsales<-100000
    pselling<-8.00
  }
}

netprofit <- vsales*(pselling-unitcost)-fixedcost
print("The expected total  profit is:")
print(netprofit)

hist(netprofit,freq=FALSE)

for(country in 1:nrow(contiguous)){
  closest_center <- 1
  closest_distance <- dist_sq(contiguous[country,],centers[1,])
  
  for(cluster in 2:N){
    d <- dist_sq(contiguous[county,],centers[clusters,])
    if(d <closest_distance){
      closest_distance <- d
      closest_center <- cluster
    }
  }
  belongs_to[county] <- cluster
}

plot(contiguous$longtitude,contiguous$latitude,type='p',col=belongs_to)

claut_of_interest <- contiguous[belongs_to==1,]
total_pop <- sum(clust_of_interest$population)
new_latitude <- sum(clust_of_interest$latitude * clust_of_interest$population)/total_pop



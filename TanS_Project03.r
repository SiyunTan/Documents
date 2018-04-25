# Name: Siyun Tan
# Course: 44-149 Scientific Computing
# Assignment # project02
# Due Date: April06
# Brief: A brief description of the assignment
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I 
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy

N <- 8
census <- read.csv('~/us_census.csv')
contiguous <- census[!census$state %in% c('AK','HI','PR'),]
chosen_countries <- sample(1:nrow(contiguous),N)
print(chosen_countries)

centers <- matrix(0,nrow=N,ncol=2)

for (i in 1:N){
  center[i][1]=contiguous[chosen_countries[i],'latitude']
  center[i][2]=contiguous$longtitude[chosen_countries[i]]
}

centers[,1]=contiguous$latitude[chosen_countries]
centers[,2]=contiguous$longtitude[chosen_countries]
centers_df=contiguous[chosen_countries,3:4]

dist_sq <- function(county,center){
  deltax <- contiguous['latitide'] - centers[1]
  deltay <- contiguous['longtitude'] - centers[2]
  deltax^x +deltay^2
}
belongs_to <- rep(0,nrow(contiguous))

for(i in 1:3){
  for(county in 1:nrow(contiguous)){
    closest_center <- -1
    closest_distance <- dist_sq(contiguous[county,],centers[1,])
  }
}

for(cluster in 2:N){
  d <- dist_sq(contiguous[county,], centers[cluster,])
  if(d <- closest_distance){
    closest_distance = d
    cloest_center = cluster
  }
  belongs_to[county] <- closest_center
}


for(country in 1:nrow(contiguous)){
  closest_center <- 1
  closest_distance <- dist_sq(contiguous[country,],centers[1,])
  
  for(cluster in 2:N){
    d <- dist_sq(contiguous[county,],centers[clusters,])
    if(d < closest_distance){
      closest_distance <- d
      closest_center <- cluster
    }
  }
  belongs_to[county] <- closest_center
}

plot(contiguous$longtitude,contiguous$latitude,type='p',col=belongs_to)

claut_of_interest <- contiguous[belongs_to==1,]
total_pop <- sum(clust_of_interest$population)
new_latitude <- sum(clust_of_interest$latitude * clust_of_interest$population)/total_pop
new_longtitude <- sum(clust_of_interest$longtitude * clust_of_interest$population)
centers[1,1] <- new_latitude


centers_df=contiguous[chosen_counties,3:4]

print(contiguous[1,])
print(centers[1,])



deltax <- contiguous[1,'latitide'] - centers[1,1]
deltay <- contiguous[1,'longtitude'] - centers[1,2]

print(deltax^2 + deltay^2)

plot(contiguous$longtitude,contiguousd)
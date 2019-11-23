##reload data, different name is given
library(MASS)
library(ggplot2)
library(tidyr)
library(cluster)
# load the data
data("Boston")
summary(Boston)

boston_scaled2 <- scale(Boston)
summary(boston_scaled2)
# class of the boston_scaled object
class(boston_scaled2)
# change the object to data frame
boston_scaled2<-as.data.frame(boston_scaled2)

#distance calculation
dist_eu<- dist(boston_scaled2)
summary(dist_eu)

#kmeans clusterin, first try with 4 clusters
km4 <-kmeans(boston_scaled2, centers = 4)
# plot the Boston dataset with clusters
pairs(boston_scaled2, col = km4$cluster)

#Find the optimal number for k
set.seed(123)
# determine the number of clusters
k_max <- 10
# calculate the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(boston_scaled2, k)$tot.withinss})
# visualize the results
qplot(x = 1:k_max, y = twcss, geom = 'line',main="WGSS and groups in K-means solution")

#optimal number of groups is 2
km <-kmeans(boston_scaled2, centers = 2)
str(km)
# plot the Boston dataset with clusters
pairs(boston_scaled2, col = km$cluster)


#MIT Analytics Edge Week 6 - Clustering Pixels

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
flower <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/flower.csv', header = FALSE)
str(flower)
flowerMatrix <- as.matrix(flower)
str(flowerMatrix)
flowerVector <- as.vector(flowerMatrix)
str(flowerVector)

#Hierarchical Clustering
distance <- dist(flowerVector, method = "euclidean")
clusterIntesity <- hclust(distance, method = "ward.D")
#The ward method is a minimum variance method, which tries to find compact and spherical clusters. We can think about it
#as trying to minimize the variance within each cluster and the distance among clusters. 

plot(clusterIntesity)
#2 or 3 clusters is reasonable here
rect.hclust(clusterIntesity, k = 3, border = "red")

#Now let's split the data into these 3 clusters
flowerClusters <- cutree(clusterIntesity, k = 3)
flowerClusters
#We see that flowerClusters is actually a vector that assigns each intesity value in the flower vector to a cluster

tapply(flowerVector, flowerClusters, mean)

#Now let us see how the image was segmented
#To output an image, we can use the image function, which takes a matrix as an input. However, flowerClusters is a 
#vector, so we need to convert it to a matrix.
#Set dimensions
dim(flowerClusters) <- c(50, 50)

image(flowerClusters, axes = FALSE)

#Check what the original image looked like
image(flowerMatrix, axes = FALSE, col = grey(seq(0, 1, length = 256)))

#Very low resolution

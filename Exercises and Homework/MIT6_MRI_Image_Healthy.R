#MIT Analytics Edge Week 6 - Clustering Healthy MRI Image

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
healthy <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/healthy.csv', header = FALSE)

healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)

#Actual MRI image
image(healthyMatrix, axes = FALSE, col = grey(seq(0, 1, length = 256)))

#There are different substances such as the gray matter, the white matter, and the cerebrospinal fluid.

#Let's see if we can isolate these via hierarchical clustering.
healthyVector <- as.vector(healthyMatrix)
distance = dist(healthyVector, method = "euclidean")

#Not enough memory
str(healthyVector)
n <- 365636
#For R to calculate the pairwise distances, it would actually need to calculate n*(n-1)/2 and then store them in the
#distance matrix.
n*(n-1)/2
#Well there are 67 billion values we're asking R to store so of course there's going to be an error. 
#Can't use hierarchical clustering

#k-means
k <- 5
set.seed(1)
#Since k-means algorithm is iterative, it could take a very long time to converge, so we should set a maximum
#number of iterations.
KMC <- kmeans(healthyVector, centers = k, iter.max = 1000) 
str(KMC)

#Output clusters
healthyClusters <- KMC$cluster
#Extract mean intensity for each cluster
KMC$centers[2] #Mean intensity of 2nd cluster

#Let's output the segmented image and see what we get
dim(healthyClusters) <- c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes = FALSE, col = rainbow(k))

#More refining maybe needs to be made to our clustering algorithm to appropriately capture all the anatomical 
#structures. But this seems like a good starting point.

#Scree Plot
KMC$withinss
#Suppose we want to determine the best number of clusters for this dataset.
#Repeat k-means with each value of number of clusters
KMC2 = kmeans(healthyVector, centers = 2, iter.max = 1000)
KMC3 = kmeans(healthyVector, centers = 3, iter.max = 1000)
KMC4 = kmeans(healthyVector, centers = 4, iter.max = 1000)
KMC6 = kmeans(healthyVector, centers = 6, iter.max = 1000)
KMC7 = kmeans(healthyVector, centers = 7, iter.max = 1000)
KMC8 = kmeans(healthyVector, centers = 8, iter.max = 1000)
KMC9 = kmeans(healthyVector, centers = 9, iter.max = 1000)
KMC10 = kmeans(healthyVector, centers = 10, iter.max = 1000)

NumClusters = seq(2, 10, 1)

SumWithinss = c(sum(KMC2$withinss), sum(KMC3$withinss), sum(KMC4$withinss), sum(KMC$withinss),
                sum(KMC6$withinss), sum(KMC7$withinss), sum(KMC8$withinss), sum(KMC9$withinss), sum(KMC10$withinss))

plot(NumClusters, SumWithinss, type = "b") #type = "b" just told the plot to give us points and lines
#Looks like 4 or 5 clusters is a good choice (the bend/elbow of the plot)

#Could have generated SumWithinss with much less effort
SumWithinss = sapply(2:10, function(x)sum(kmeans(healthyVector, centers = x, iter.max = 1000)$withinss))
SumWithinss

#The real question is, can we use the clusters we found by the k-means algorithm on the healthy MRI image to 
#identify tumors in another MRI image of a sick patient?

tumor <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/tumor.csv', header = FALSE)

tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)

#Treat the healthy vector (previous file/problem) as the training set and the tumor vector as the testing set.

library(flexclust)
#This package contains the object class KCCA (K-Centroids Cluster Analysis)
#We need to convert the information from the clustering algorithm to an object of the class KCCA.
#This conversion is needed before we can use the predict function on the test set tumor vector.
KMC.kcca <- as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)
#Now the tumorClusters is a vector that assigns a value 1 through 5 to each of the intensity values in the tumor
#vector, as predicted by the k-means algorithm. 

#To output the segmented image 
dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col = rainbow(k))

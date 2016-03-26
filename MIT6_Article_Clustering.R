#MIT Analytics Edge Week 6 - Clustering Articles

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
dailykos <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/dailykos.csv')
str(dailykos)

#Hierarchical Clustering

distance <- dist(dailykos, method = "euclidean")
kosclust <- hclust(distance, method = "ward.D")

#Plot dendrogram
plot(kosclust)

#2 or 3 clusters are good choices according to the dendrogram because there is a lot of space between the horizontal
#lines in the dendrogram  in those cut off spots. 5 and 6 clusters don't seem good because there is very little space.
#However, when thinking about this application, we're trying to cluster news articles or blog posts into groups.
#This can be used to show readers categories to choose from when trying to decide what to read. 7 clusters is reasonable
#according to the dendrogram, and also seems reasonable for the application.

#Split data into 7 clusters
kosClusters <- cutree(kosclust, k = 7)

#Create 7 new datasets each containing the observations from one cluster.
kosCluster1 <- subset(dailykos, kosClusters == 1)  
kosCluster2 <- subset(dailykos, kosClusters == 2)
kosCluster3 <- subset(dailykos, kosClusters == 3)
kosCluster4 <- subset(dailykos, kosClusters == 4)
kosCluster5 <- subset(dailykos, kosClusters == 5)
kosCluster6 <- subset(dailykos, kosClusters == 6)
kosCluster7 <- subset(dailykos, kosClusters == 7)

#How many observations in cluster 3?
nrow(kosCluster3)

#Which cluster has the most observations?
nrow(kosCluster7)
nrow(kosCluster6)
nrow(kosCluster5)
nrow(kosCluster4)
nrow(kosCluster2)
nrow(kosCluster1)

#Or
table(kosClusters)

#Or
#Given a vector assigning groups like kosClusters, you could split dailykos into clusters by:
dkCluster = split(dailykos, kosClusters)
dkCluster[[1]] #to access cluster 1

#Look at top 6 words in each cluster
tail(sort(colMeans(kosCluster1)))
#This computes the mean frequency values of each of the words in cluster 1, where each cluster is composed of
#news articles and blog posts.
tail(sort(colMeans(kosCluster2)))
tail(sort(colMeans(kosCluster3)))
tail(sort(colMeans(kosCluster4)))
tail(sort(colMeans(kosCluster5)))
tail(sort(colMeans(kosCluster6)))
tail(sort(colMeans(kosCluster7)))


#K-Means Clustering
set.seed(1000)
k <- 7
kosKMC <- kmeans(dailykos, centers = k)
kkmclust1 <- subset(dailykos, kosKMC$cluster == 1)
kkmclust2 <- subset(dailykos, kosKMC$cluster == 2)
kkmclust3 <- subset(dailykos, kosKMC$cluster == 3)
kkmclust4 <- subset(dailykos, kosKMC$cluster == 4)
kkmclust5 <- subset(dailykos, kosKMC$cluster == 5)
kkmclust6 <- subset(dailykos, kosKMC$cluster == 6)
kkmclust7 <- subset(dailykos, kosKMC$cluster == 7)

#How many observations in cluster 3?
nrow(kkmclust3)
nrow(kkmclust1)
nrow(kkmclust2)
nrow(kkmclust4)
nrow(kkmclust5)
nrow(kkmclust6)
nrow(kkmclust7)

#Or
table(kosKMC$cluster)

#Output the 6 most frequent words in each cluster
tail(sort(colMeans(kkmclust1)))
tail(sort(colMeans(kkmclust2)))
tail(sort(colMeans(kkmclust3)))
tail(sort(colMeans(kkmclust4)))
tail(sort(colMeans(kkmclust5)))
tail(sort(colMeans(kkmclust6)))
tail(sort(colMeans(kkmclust7)))

#Compare k-means vs hierarchical cluster assignment
table(kosClusters, kosKMC$cluster)
#We can see here that 116 (80.6%) of the observations in k-means cluster 2 are in hierarchical cluster 7.

#Which hierarchical cluster best corresponds to k-means cluster 3? Cluster 5 (61%)
(171)/(171+42+64+3)

#Which hierarchical clusters best corresponds to k-means cluster 7? Only 39.9% of the observations in k-means
#cluster 7 are in hierarchical cluster4 which is the most. Since no more than 39.9% falls into any one cluster, no
#such cluster is best. 
123/(123+111+1+24+39+10)

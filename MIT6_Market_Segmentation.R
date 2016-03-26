#MIT Analytics Edge Week 6 - Market Segmentation for Airlines

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
airlines <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/AirlinesCluster.csv')

summary(airlines)

#Normalizing the data
library(caret)
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)

summary(airlinesNorm)

#Hierarchical Clustering
ALNdistance <- dist(airlinesNorm, method = "euclidean")
ALNhClusters <- hclust(ALNdistance, method = "ward.D")
plot(ALNhClusters)
rect.hclust(ALNhClusters, k = 7, border = "red")

AClusters <- cutree(ALNhClusters, k = 5)
table(AClusters)

#Means across clusters of unnormalized variables
tapply(airlines$Balance, AClusters, mean)
tapply(airlines$QualMiles, AClusters, mean)
tapply(airlines$BonusMiles, AClusters, mean)
tapply(airlines$BonusTrans, AClusters, mean)
tapply(airlines$FlightMiles, AClusters, mean)
tapply(airlines$FlightTrans, AClusters, mean)
tapply(airlines$DaysSinceEnroll, AClusters, mean)

#Cluster 1 seems to be infrequent but loyal customers
#Cluster 2 seems to be customers who have accumulated a large number of miles and the ones with the largest number of
#flight transactions.
#Cluster 3 seems to be customers who  have accumulated large amounts of miles through non-flight transactions.
#Cluster 4 seems to be customers who are relatively new who seem to be accumulating miles, mostly through non-flight
#transactions.
#Cluster 5 seems to be customers who are relatively new and don't use the airline very often.

#K-Means Clustering
set.seed(88)
k <- 5
ALNhKMC <- kmeans(airlinesNorm, center = k, iter.max = 1000)
table(ALNhKMC$cluster)
ALNhKMC$centers #to see centroids across clusters and variables of normalized data

tapply(airlines$Balance, ALNhKMC$cluster, mean)
tapply(airlines$QualMiles, ALNhKMC$cluster, mean)
tapply(airlines$BonusMiles, ALNhKMC$cluster, mean)
tapply(airlines$BonusTrans, ALNhKMC$cluster, mean)
tapply(airlines$FlightMiles, ALNhKMC$cluster, mean)
tapply(airlines$FlightTrans, ALNhKMC$cluster, mean)
tapply(airlines$DaysSinceEnroll, ALNhKMC$cluster, mean)

#Note: clusters are not displayed in a meaningful order
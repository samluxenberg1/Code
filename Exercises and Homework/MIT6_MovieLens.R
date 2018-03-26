#MIT Analytics Edge Week 6 - MovieLens

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
movies <- read.table("movieLense.txt", header = FALSE, sep = "|", quote = "\"")
str(movies)
colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure",
                      "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir",
                      "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)
#Remove specific variables we're not using
movies$ID <- NULL
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL

#Remove duplicate entries
movies <- unique(movies)
str(movies)

#Hierarchical Clustering
#Only want to cluster movies on the genre variable, not on the Title variable

#Compute distances between each point
distances <- dist(movies[2:20], method = "euclidean")

#Cluster movies
clusterMovies <- hclust(distances, method = "ward.D")
#The ward method cares about the distance between clusters using the centroid distance and also the variance in each
#of the clusters.
plot(clusterMovies)
#Difficult to read because in hierarchical clustering each data point starts as its own cluster. So with over a 
#thousand different points, it lacks clarity.

#Looks like maybe 2, 3, or 4 movies look like good numbers to choose for the number of cluster. But let's also 
#keep in mind the application at hand. We probably want more than 4 clusters of movies to make recommendations to 
#customers. 
#There's a nice break of 10 clusters closer to the bottom which would be better for our application.
#We could select even more clusters if we wanted very specific groups. If you want a lot of clusters, it's hard to
#pick the right number from the dendrogram. You have to use your understanding of the problem to pick the number
#of clusters  

#Select 10 clusters

#Label each of the data points according to which cluster is belongs to
clusterGroups <- cutree(clusterMovies, k = 10)

#Let's figure out what these clusters are like
#Use tapply function to compute the percentage of movies in each genre and cluster
tapply(movies$Action, clusterGroups, mean)
#This divides our data points into 10 clusters and then computes the average value of the action variables for each
#cluster. Since the Action variable is binary, by computing the average, we're finding the percentage of action
#films in each cluster. 

tapply(movies$Romance, clusterGroups, mean)

#You can repeat this for each genre. If you do, you can create a large table to better analyze the clusters.

subset(movies, Title == "Men in Black (1997)")
#Which cluster did Men in Black go into?
clusterGroups[257]
#This makes sense since cluster two is the action, adventure, SciFi cluster

#Create new dataset with just the movies from cluster 2
cluster2 <- subset(movies, clusterGroups == 2)
cluster2$Title[1:10]
#So accordig to our cluster algorithm, good movies to recommend would be movies like Apollo 13 and Jurassic Park


#Alternative approach to finding cluster centroids
#Here, only one command for each cluster instead of only one command for each variable.
colMeans(subset(movies[2:20], clusterGroups == 1))
#However, if you have a lot of clusters, this approach is not much more efficient than tapply.
#Split the data into subsets and use lapply
spl = split(movies[2:20], clusterGroups)
#Use spl to access different clusters
spl[[1]] #same as subset(movies[2:20], clusterGroups == 1)
colMeans(spl[[1]]) #this outputs the centroid of cluster 1

#Even easier approach uses lapply
#Want to output cluster centroids of all clusters
lapply(spl, colMeans)
#Note if you have a variable called "split" in the current R session, you will need to remove it with rm(split)
#so that you can use the split function. 

#What if we had picked 2 clusters?
clusterGroups2 <- cutree(clusterMovies, k = 2)
spl2 = split(movies[2:20], clusterGroups2)
lapply(spl2, colMeans)

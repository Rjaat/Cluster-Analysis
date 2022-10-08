library(tidyverse)
shape1_data <- read.delim("shape1.txt") #for reading tab-separated value files or text files
shape1_data

#Data cleaning
library(ggplot2) #for visualization
ggplot(shape1_data, aes(x = X15.55, y = X28.65)) + geom_point() #point geom is used to create scatterplots
summary(shape1_data)

#scaling
# mostly we scale each column in the data to zero mean and unit standard deviation (z-scores)
scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))
shape1_scaled <- shape1_data %>% scale_numeric()
summary(shape1_scaled)


#CLUSTERING

# 1. k-means clustering
#k-means implicitly assumes Euclidean distances.
# we use k=4 clusters and run the algorithm 10 times with random initialized centroids.
tic()
km <- kmeans(shape1_scaled, centers = 3, nstart = 10)
#km is an object, implemented as a list
km

#each data row in km can b accessed using km$cluster
#here we added km as column in scaled data
shape1_clustered <- shape1_scaled %>% add_column(cluster = factor(km$cluster))
shape1_clustered

ggplot(shape1_clustered, aes(x = X15.55, y = X28.65, color = cluster)) + geom_point()


#Adding the centroids to the plot
#centroid is the imaginary or real location representing the center of the cluster
centroids <- as_tibble(km$centers, rownames = "cluster")
centroids
ggplot(shape1_clustered, aes(x=X15.55, y = X28.65, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x=X15.55, y = X28.65, color = cluster), shape = 3, size = 10)


#factoextra package for visualization
library(factoextra)
fviz_cluster(km, data = shape1_scaled, centroids = TRUE, repel = TRUE, ellipse.type = "norm")

toc()
#extracting single cluster
#We need to filter the rows corresponding to the cluster index

cluster2 <- shape1_clustered %>% filter(cluster == 2)
cluster2
summary(cluster2)
ggplot(cluster2, aes(x = X15.55, y = X28.65)) + geom_point() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))

#cluster with 8 centers
fviz_cluster(kmeans(shape1_scaled, centers = 8), data = shape1_scaled,
             centroids = TRUE,  geom = "point", ellipse.type = "norm")
#thus, here are too few points to calculate an ellipse



# 2. Hierarchical clustering
#it starts with a distance matrix. dist() defaults to method=“Euclidean"
#distance matrix becomes very large quickly 
#time & size complexitiy is O(n^2)
tic()
d <- dist(shape1_scaled)
hc <- hclust(d, method = "complete")
#hclust() implements agglomerative hierarchical clustering
#in general, the merges and splits are determined in a greedy manner
#Hierarchical clustering does not return cluster assignments but a dendrogram. 
#The standard plot(), plots the dendrogram
plot(hc)
fviz_dend(hc, k = 5)  #number of clusters to visualize in dendogram
toc()


# 3. Density-Based Spatial Clustering of Applications with Noise
library(dbscan) #Density-Based Spatial Clustering of Applications with Noise.
#It groups together points that are closely packed together and treats points in low-density regions as outliers.

kNNdistplot(shape1_scaled, k = 3)
abline(h= .32, col="red") #adds one or more straight lines through the current plot
tic()
db <- dbscan(shape1_scaled, eps = .32, minPts = 4) #minpts defines how many points in the epsilon neighborhood are needed to make a point a core point
db
str(db)
ggplot(shape1_scaled %>% add_column(cluster = factor(db$cluster)),
       aes(x = X15.55, y = X28.65, color = cluster)) + geom_point()
#here Cluster 0 represents outliers

fviz_cluster(db, shape1_scaled, geom = "point")
toc()




#Partitioning Around Medoids (PAM) --> to solve k-medoids problem
#medoid is most central data point in the middle of cluster
# similiar to k-means but uses medoids instead of centroids to represent cluster
#like hierarchical clustering, it typically works with precompound distace matrix
#Advantage is, we can use any distance matrix not just Euclidean matrix 
library(cluster)
tic()
d <- dist(shape1_scaled)
str(d)
p <- pam(d, k = 4)
p
shape1_clustered <- shape1_scaled %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(shape1_scaled[p$medoids, ], rownames = "cluster")
medoids
ggplot(shape1_clustered, aes(x = X15.55, y = X28.65, color = cluster)) + geom_point() +
  geom_point(data = medoids, aes(x = X15.55, y = X28.65, color = cluster), shape = 3, size = 7)

fviz_cluster(c(p, list(data = shape1_scaled)), geom = "point", ellipse.type = "norm")
toc()



#Find Optimal Number of Clusters for k-means
#ggplot(shape1_scaled, aes(x=X15.55, y=X28.65)) + geom_point()
set.seed(1234)
ks <- 2:10
k <- clusGap(shape1_scaled, FUN = kmeans,  nstart = 10, K.max = 10)
k
plot(k)
#Silhouette plot for kmeans
plot(silhouette(db$cluster, d))
fviz_silhouette(silhouette(km$cluster, d))




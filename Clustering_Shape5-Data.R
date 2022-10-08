library(tidyverse)
shape5_data <- read.delim("shape5.txt") 
shape5_data

#Data cleaning
library(ggplot2) 
ggplot(shape5_data, aes(x = X25.0514, y = X5.7475)) + geom_point()
summary(shape5_data)

scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))
shape5_scaled <- shape5_data %>% scale_numeric()
summary(shape5_scaled)


#CLUSTERING

# 1. k-means clustering
library(tictoc)
tic()
km <- kmeans(shape5_scaled, centers = 3, nstart = 10)

km

shape5_clustered <- shape5_scaled %>% add_column(cluster = factor(km$cluster))
shape5_clustered

ggplot(shape5_clustered, aes(x = X25.0514, y = X5.7475, color = cluster)) + geom_point()
toc()

centroids <- as_tibble(km$centers, rownames = "cluster")
centroids
ggplot(shape5_clustered, aes(x=X25.0514, y = X5.7475, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x=X25.0514, y = X5.7475, color = cluster), shape = 3, size = 10)


#factoextra package for visualization
library(factoextra)
fviz_cluster(km, data = shape5_scaled, centroids = TRUE, repel = TRUE, ellipse.type = "norm")

#extracting single cluster
cluster2 <- shape5_clustered %>% filter(cluster == 2)
cluster2
summary(cluster2)
ggplot(cluster2, aes(x = X25.0514, y = X5.7475)) + geom_point() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))

#cluster with 8 centers
fviz_cluster(kmeans(shape5_scaled, centers = 8), data = shape5_scaled,
             centroids = TRUE,  geom = "point", ellipse.type = "norm")



# 2. Hierarchical clustering
tic()
d <- dist(shape5_scaled)
hc <- hclust(d, method = "complete")

plot(hc)
fviz_dend(hc, k = 4)  #k is number of clusters to visualize in dendogram

toc()


# 3. Density-Based Spatial Clustering of Applications with Noise
library(dbscan) 
kNNdistplot(shape5_scaled, k = 3)
tic()
db <- dbscan(shape5_scaled, eps = .32, minPts = 4) 
db
str(db)
ggplot(shape5_scaled %>% add_column(cluster = factor(db$cluster)),
       aes(x = X25.0514, y = X5.7475, color = cluster)) + geom_point()


fviz_cluster(db, shape5_scaled, geom = "point")

toc()



#Partitioning Around Medoids (PAM) --> to solve k-medoids problem

library(cluster)
tic()
d <- dist(shape5_scaled)
str(d)
p <- pam(d, k = 4)
p
shape5_clustered <- shape5_scaled %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(shape5_scaled[p$medoids, ], rownames = "cluster")
medoids
ggplot(shape5_clustered, aes(x = X25.0514, y = X5.7475, color = cluster)) + geom_point() +
  geom_point(data = medoids, aes(x = X25.0514, y = X5.7475, color = cluster), shape = 3, size = 7)

fviz_cluster(c(p, list(data = shape5_scaled)), geom = "point", ellipse.type = "norm")
toc()

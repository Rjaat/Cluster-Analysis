library(tidyverse)
shape7_data <- read.delim("shape7.txt") 
shape7_data

#Data cleaning
library(ggplot2) 
ggplot(shape7_data, aes(x = X0.85, y = X17.45)) + geom_point()
summary(shape7_data)

scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))
shape7_scaled <- shape7_data %>% scale_numeric()
summary(shape7_scaled)


#CLUSTERING

# 1. k-means clustering
tic()
km <- kmeans(shape7_scaled, centers = 5, nstart = 10)

km

shape7_clustered <- shape7_scaled %>% add_column(cluster = factor(km$cluster))
shape7_clustered

ggplot(shape7_clustered, aes(x = X0.85, y = X17.45, color = cluster)) + geom_point()


centroids <- as_tibble(km$centers, rownames = "cluster")
centroids
ggplot(shape7_clustered, aes(x=X0.85, y = X17.45, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x=X0.45, y = X17.45, color = cluster), shape = 3, size = 10)


#factoextra package for visualization
library(factoextra)
fviz_cluster(km, data = shape7_scaled, centroids = TRUE, repel = TRUE, ellipse.type = "norm")
toc()
#extracting single cluster
cluster2 <- shape7_clustered %>% filter(cluster == 2)
cluster2
summary(cluster2)
ggplot(cluster2, aes(x = X0.85, y = X17.45)) + geom_point() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))

#cluster with 8 centers
fviz_cluster(kmeans(shape7_scaled, centers = 8), data = shape7_scaled,
             centroids = TRUE,  geom = "point", ellipse.type = "norm")



# 2. Hierarchical clustering
tic()
d <- dist(shape7_scaled)
hc <- hclust(d, method = "complete")

plot(hc)
fviz_dend(hc, k = 6)  #k is number of clusters to visualize in dendogram
toc()


# 3. Density-Based Spatial Clustering of Applications with Noise
library(dbscan) 
kNNdistplot(shape7_scaled, k = 4)
tic()
db <- dbscan(shape7_scaled, eps = .32, minPts = 7) 
db
str(db)
ggplot(shape7_scaled %>% add_column(cluster = factor(db$cluster)),
       aes(x = X0.85, y = X17.45, color = cluster)) + geom_point()


fviz_cluster(db, shape7_scaled, geom = "point")

toc()



#Partitioning Around Medoids (PAM) --> to solve k-medoids problem

library(cluster)
tic()
d <- dist(shape7_scaled)
str(d)
p <- pam(d, k = 5)
p
shape7_clustered <- shape7_scaled %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(shape7_scaled[p$medoids, ], rownames = "cluster")
medoids
ggplot(shape7_clustered, aes(x = X0.85, y = X17.45, color = cluster)) + geom_point() +
  geom_point(data = medoids, aes(x = X0.85, y = X17.45, color = cluster), shape = 3, size = 7)

fviz_cluster(c(p, list(data = shape7_scaled)), geom = "point", ellipse.type = "norm")
toc()

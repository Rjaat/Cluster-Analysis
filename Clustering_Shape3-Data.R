library(tidyverse)
shape8_data <- read.delim("shape3.txt") 
shape8_data

#Data cleaning
library(ggplot2) 
ggplot(shape8_data, aes(x = X1.85, y = X27.8)) + geom_point()
summary(shape8_data)

scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))
shape8_scaled <- shape8_data %>% scale_numeric()
summary(shape8_scaled)


#CLUSTERING

# 1. k-means clustering
tic()
km <- kmeans(shape8_scaled, centers = 5, nstart = 10)

km

shape8_clustered <- shape8_scaled %>% add_column(cluster = factor(km$cluster))
shape8_clustered

ggplot(shape8_clustered, aes(x = X1.85, y = X27.8, color = cluster)) + geom_point()


centroids <- as_tibble(km$centers, rownames = "cluster")
centroids
ggplot(shape8_clustered, aes(x=X1.85, y = X27.8, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x=X1.85, y = X27.8, color = cluster), shape = 3, size = 10)


#factoextra package for visualization
library(factoextra)
fviz_cluster(km, data = shape8_scaled, centroids = TRUE, repel = TRUE, ellipse.type = "norm")
toc()
#extracting single cluster
cluster2 <- shape8_clustered %>% filter(cluster == 2)
cluster2
summary(cluster2)
ggplot(cluster2, aes(x = X1.85, y = X27.8)) + geom_point() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))

#cluster with 8 centers
fviz_cluster(kmeans(shape8_scaled, centers = 8), data = shape8_scaled,
             centroids = TRUE,  geom = "point", ellipse.type = "norm")



# 2. Hierarchical clustering
tic()
d <- dist(shape8_scaled)
hc <- hclust(d, method = "complete")

plot(hc)
fviz_dend(hc, k = 6)  #k is number of clusters to visualize in dendogram
toc()


# 3. Density-Based Spatial Clustering of Applications with Noise
library(dbscan) 
kNNdistplot(shape8_scaled, k = 5)
tic()
db <- dbscan(shape8_scaled, eps = .32, minPts = 6) 
db
str(db)
ggplot(shape8_scaled %>% add_column(cluster = factor(db$cluster)),
       aes(x = X1.85, y = X27.8, color = cluster)) + geom_point()


fviz_cluster(db, shape8_scaled, geom = "point")
toc()




#Partitioning Around Medoids (PAM) --> to solve k-medoids problem

library(cluster)
tic()
d <- dist(shape8_scaled)
str(d)
p <- pam(d, k = 5)
p
shape8_clustered <- shape8_scaled %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(shape8_scaled[p$medoids, ], rownames = "cluster")
medoids
ggplot(shape8_clustered, aes(x = X1.85, y = X27.8, color = cluster)) + geom_point() +
  geom_point(data = medoids, aes(x = X1.85, y = X27.8, color = cluster), shape = 3, size = 7)

fviz_cluster(c(p, list(data = shape8_scaled)), geom = "point", ellipse.type = "norm")
toc()

library(tidyverse)
shape4_data <- read.delim("shape4.txt") 
shape4_data

#Data cleaning
library(ggplot2) 
ggplot(shape4_data, aes(x = X31.95, y = X7.95)) + geom_point()
summary(shape4_data)

scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))
shape4_scaled <- shape3_data %>% scale_numeric()
summary(shape4_scaled)


#CLUSTERING

# 1. k-means clustering
tic()
km <- kmeans(shape4_scaled, centers = 3, nstart = 10)

km

shape4_clustered <- shape4_scaled %>% add_column(cluster = factor(km$cluster))
shape4_clustered

ggplot(shape4_clustered, aes(x = X31.95, y = X7.95, color = cluster)) + geom_point()


centroids <- as_tibble(km$centers, rownames = "cluster")
centroids
ggplot(shape4_clustered, aes(x=X31.95, y = X7.95, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x=X31.95, y = X7.95, color = cluster), shape = 3, size = 10)
toc()

#factoextra package for visualization
library(factoextra)
fviz_cluster(km, data = shape4_scaled, centroids = TRUE, repel = TRUE, ellipse.type = "norm")

#extracting single cluster
cluster2 <- shape4_clustered %>% filter(cluster == 2)
cluster2
summary(cluster2)
ggplot(cluster2, aes(x = X31.95, y = X7.95)) + geom_point() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))

#cluster with 8 centers
fviz_cluster(kmeans(shape4_scaled, centers = 8), data = shape4_scaled,
             centroids = TRUE,  geom = "point", ellipse.type = "norm")



# 2. Hierarchical clustering
tic()
d <- dist(shape4_scaled)
hc <- hclust(d, method = "complete")

plot(hc)
fviz_dend(hc, k = 5)  #k is number of clusters to visualize in dendogram
toc()


# 3. Density-Based Spatial Clustering of Applications with Noise
library(dbscan) 

kNNdistplot(shape4_scaled, k = 2)
tic()
db <- dbscan(shape4_scaled, eps = .32, minPts = 6) 
db
str(db)
ggplot(shape4_scaled %>% add_column(cluster = factor(db$cluster)),
       aes(x = X31.95, y = X7.95, color = cluster)) + geom_point()


fviz_cluster(db, shape4_scaled, geom = "point")
toc()




#Partitioning Around Medoids (PAM) --> to solve k-medoids problem

library(cluster)
tic()
d <- dist(shape4_scaled)
str(d)
p <- pam(d, k = 3)
p
shape4_clustered <- shape4_scaled %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(shape4_scaled[p$medoids, ], rownames = "cluster")
medoids
ggplot(shape4_clustered, aes(x = X31.95, y = X7.95, color = cluster)) + geom_point() +
  geom_point(data = medoids, aes(x = X31.95, y = X7.95, color = cluster), shape = 3, size = 7)

fviz_cluster(c(p, list(data = shape4_scaled)), geom = "point", ellipse.type = "norm")

toc()

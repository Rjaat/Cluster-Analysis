library(tidyverse)
shape6_data <- read.delim("shape6.txt") 
shape6_data

#Data cleaning
library(ggplot2) 
ggplot(shape6_data, aes(x = X9.802, y = X10.132)) + geom_point()
summary(shape6_data)

scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))
shape6_scaled <- shape6_data %>% scale_numeric()
summary(shape6_scaled)


#CLUSTERING

# 1. k-means clustering
tic()
km <- kmeans(shape6_scaled, centers = 4, nstart = 10)

km

shape6_clustered <- shape6_scaled %>% add_column(cluster = factor(km$cluster))
shape6_clustered

ggplot(shape6_clustered, aes(x = X9.802, y = X10.132, color = cluster)) + geom_point()


centroids <- as_tibble(km$centers, rownames = "cluster")
centroids
ggplot(shape6_clustered, aes(x=X9.802, y = X10.132, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x=X9.802, y = X10.132, color = cluster), shape = 3, size = 10)


#factoextra package for visualization
library(factoextra)
fviz_cluster(km, data = shape6_scaled, centroids = TRUE, repel = TRUE, ellipse.type = "norm")
toc()
#extracting single cluster
cluster2 <- shape6_clustered %>% filter(cluster == 2)
cluster2
summary(cluster2)
ggplot(cluster2, aes(x = X9.802, y = X10.132)) + geom_point() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))

#cluster with 8 centers
fviz_cluster(kmeans(shape6_scaled, centers = 8), data = shape6_scaled,
             centroids = TRUE,  geom = "point", ellipse.type = "norm")



# 2. Hierarchical clustering
tic()
d <- dist(shape6_scaled)
hc <- hclust(d, method = "complete")

plot(hc)
fviz_dend(hc, k = 4)  #k is number of clusters to visualize in dendogram

toc()

# 3. Density-Based Spatial Clustering of Applications with Noise
library(dbscan) 
tic()
kNNdistplot(shape6_scaled, k = 3)

db <- dbscan(shape6_scaled, eps = .32, minPts = 4) 
db
str(db)
ggplot(shape6_scaled %>% add_column(cluster = factor(db$cluster)),
       aes(x = X9.802, y = X10.132, color = cluster)) + geom_point()


fviz_cluster(db, shape6_scaled, geom = "point")

toc()



#Partitioning Around Medoids (PAM) --> to solve k-medoids problem

library(cluster)
tic()
d <- dist(shape6_scaled)
str(d)
p <- pam(d, k = 6)
p
shape6_clustered <- shape6_scaled %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(shape6_scaled[p$medoids, ], rownames = "cluster")
medoids
ggplot(shape6_clustered, aes(x = X9.802, y = X10.132, color = cluster)) + geom_point() +
  geom_point(data = medoids, aes(x = X9.802, y = X10.132, color = cluster), shape = 3, size = 7)

fviz_cluster(c(p, list(data = shape6_scaled)), geom = "point", ellipse.type = "norm")
toc()

library(tidyverse)
shape2_data <- read.delim("shape2.txt")
shape2_data

library(ggplot2)
ggplot(shape2_data, aes(x = X26.75,y= X22.15)) + geom_point()
summary(shape2_data)

#scaling
scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))
shape2_scaled <- shape2_data %>% scale_numeric()
summary(shape2_scaled)

#k-means clustering
tic()
km <- kmeans(shape2_scaled, centers = 3, nstart = 10)
km
shape2_clustered <- shape2_scaled %>% add_column(cluster = factor(km$cluster))
shape2_clustered

ggplot(shape2_clustered, aes(x = X26.75,y= X22.15, color = cluster)) + geom_point()

centroids <- as_tibble(km$centers, rownames = "cluster")
centroids
ggplot(shape2_clustered, aes(x = X26.75,y= X22.15, color = cluster)) + geom_point() +
  geom_point(data = centroids, aes(x = X26.75,y= X22.15, color = cluster), shape = 3, size = 10)
toc()
library(factoextra)
fviz_cluster(km, data = shape2_scaled, centroids = TRUE, repel = TRUE, ellipse.type = "norm")

# Hierarchical clustering
tic()
d <- dist(shape2_scaled)
hc <- hclust(d, method = "complete")
plot(hc)
fviz_dend(hc, k = 4)
toc()
# Density-Based Spatial Clustering of Applications with Noise
library(dbscan)
tic()
kNNdistplot(shape2_scaled, k = 5)
db <- dbscan(shape2_scaled, eps = .32, minPts = 3)
db
str(db)
ggplot(shape2_scaled %>% add_column(cluster = factor(db$cluster)),
       aes(x = X26.75,y= X22.15, color = cluster)) + geom_point()

fviz_cluster(db, shape2_scaled, geom = "point")
toc()
#k-medoids
library(cluster)
tic()
d <- dist(shape2_scaled)
str(d)
p <- pam(d, k = 3)
p
shape2_clustered <- shape2_scaled %>% add_column(cluster = factor(p$cluster))

medoids <- as_tibble(shape2_scaled[p$medoids, ], rownames = "cluster")
medoids

fviz_cluster(c(p, list(data = shape2_scaled)), geom = "point", ellipse.type = "norm")
toc()

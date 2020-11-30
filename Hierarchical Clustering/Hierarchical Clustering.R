#HIERARCHICAL CLUSTERING (unsupervised learning)
library(datasets)
?attitude

data <- attitude[, c(3,4)]
#we are using only 2 variables for learning purposes (see how clusters change)

distance <- dist(data)
clusters <- hclust(distance, method="complete")
#complete link-max distance of all points
clusters

plot(clusters)

clusterCut <- cutree(clusters, k=6) #cutting based on num of clusters
clusterCut
plot(data, col = clusterCut)
#cutree(clusters, h=20) #cutting based on the height

#EVALUATING THE MODEL using silhoutte 
library(cluster)
# ?silhouette
ss<-silhouette(clusterCut, dist(data))
mean(ss[,3])


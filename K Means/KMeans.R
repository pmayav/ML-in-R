#K-MEANS (unsupervised learning)
library(datasets)
?attitude

#all variables are numercial so we don't have to convert them to dummy variables
data <- attitude[, c(3,4)]
#we are using only 2 variables for learning purposes (see how clusters change)

plot(data, main="% of favourable responses to Learning and Privileges ")

#no package needed for k-means
set.seed(7)
km1 <- kmeans(data, 2 , nstart= 100) 
  #2 clusters
  #nstarts: num of times we want to run kmeans using different initial centrois
      #we repet kmeans 100 times and model return best one 
km1
#cluster means - centroids of each cluster
#WITHIN cluster sum of squares by cluster - variances within each cluster
    # -- want this to be 100%-- around (70-80% is good)
#SS-sum of squares
#total_SS = total variation of dataset
#BETWEEN_SS = variation between 2 clusters---- want this to be as large as possible 

km1$cluster
km1$centers
km1$withinss
km1$betweenss
km1$size

plot(data, col=(km1$cluster), main="K-means result with 2 clusters", pch=20, cex=2)

#BEST VALUE OF K - scree plot

mydata <- data
wss <- (nrow(mydata) -1)*sum(apply(mydata, 2, var)) #total variance
for (i in 1:15)
  wss[i] <-  sum(kmeans(mydata, centers = i)$withinss)

plot(1:15, wss, type = "b", xlab= "Num of clusters", ylab="within groups sum of squares", pch=20, cex=2)
#we can see after k=6 reduction of variation is not that significant
#thus, we can pick k=6 as the optimal num of k 

set.seed(7)
km2 <- kmeans(data, 6, nstart=100)
km2
#we can see "Within cluster sum of squares by cluster in" increased significantly
col<- (km2$cluster +1)
plot(data, col = col, main ="K-means result with 6 clusters")
points(km2$centers, col=col, pch=19, cex=2)
#bigger points are the centroids

#cluster 1 instances
cluster1 <- data[km2$cluster == 1 , ]
cluster1
#examine data for each of the clusters
#important variables-- variation is higher

#SILHOUETTE MEASURE -evaluate quality of clusters, works for any clustering method.
#a(i)= average distance of i from all the points in the same cluster
#b(i)= average distance of i from all the points in different clusters
#s(i) = b(i) - a(i) ) / max( a(i), b(i) )
  #[-1,1]. If close to 1, clustering is good -> clusters are NOT similar
  #if close  to 0, we can't tell if i is in the right cluster or not... points are too similar
  #if close to -1, a(i) is larger than b(i).. i is in the wrong cluster. 
library(cluster)
# ?silhouette
avg_sil <- function(k)
{
  kmModel<- kmeans(data, centers=k, nstart=100)
  ss <- silhouette(kmModel$cluster, dist(data))
  mean(ss[,3])
}
avg_sil(6) #avg silhouette using 6 clusters




library(readxl)
data <- read_excel("~/Downloads/prospect.xls")
head(data)

#exclude location and ID
data <- data[,-1]
data<- data[,-6]

summary(data)
Data <- data[complete.cases(data),]
Data_num <- Data[-c(4:5, 7)] #df with num variables only

#normalize the data
num_var <- unlist(lapply(Data_num, is.numeric))
num_var
Data_norm <- Data_num[, num_var]
min <- apply(Data_norm, 2, min, na.rm = TRUE)
max <- apply(Data_norm, 2, max, na.rm = TRUE)
Data_scaled <- scale(Data_norm, center = min, scale = max - min)
summary(Data_scaled)

#convert categorical variables to dummy variables
fac_var <- !num_var
fac_var
fac_var <- as.logical(fac_var)
fac_var
library(psych)
Data_fac <- as.data.frame(lapply(Data_num[,fac_var], dummy.code))

#combine normalized data and original dummy variables
Data_clean <- data.frame(Data_scaled, Data_fac, Data$`FICO>=700`, Data$OWNHOME, Data$MARRIED)
View(Data_clean)

set.seed(123)
kmModel <- kmeans(Data_clean, 4, nstart=100)
#kmModel

# The number of instnces in each cluster is:
kmModel$size

# The cluster means, aka centroids, are:
kmModel$centers

# The variances within clusters are:
kmModel$withinss

# The variance between clusters is:
kmModel$betweenss

#characteristics of each cluster
Cluster1 <- Data_clean[kmModel$cluster ==1, ]
summary(Cluster1)

#Cluster 1 is entirely Male, and there are no occurrences of Climate 20. 
#Occurrences are basically split between Climate 30 and 10. 
#A little over half of occurrences are Married, but the majority do not own a home.

Cluster2 <- Data_clean[kmModel$cluster ==2, ]
summary(Cluster2)
#Cluster 2 is entirely Female, and there are no occurrences of Climate 20. 
#Occurrences are basically split between Climate 30 and 10. 
#A little over half of occurrences are Married, but the majority do not own a home. 
#However, more own a home than in Cluster 1. Income mean is lower than in Cluster 1, 
#but Age mean is higher.

Cluster3 <- Data_clean[kmModel$cluster ==3, ]
summary(Cluster3)
#Cluster 3 is entirely Male, and all occurrences are Climate 20. 
#No occurrences are in Climate 30 or 10. Again, a little over half of occurrences are Married, 
#but the majority do not own a home. More own a home than in Cluster 1 and 2 though. 
#Income mean is the highest yet of Clusters 1-3.

Cluster4 <- Data_clean[kmModel$cluster ==4, ]
summary(Cluster4)
#Cluster 4 is entirely Male, and all occurrences are in Climate 20. 
#Like Cluster 3, no occurrences are in Climate 30 and 10. 
#A little over half of occurrences are Married, and the mean here is the highest of all 4 Clusters. 
#Still, the majority do not own a home. Age mean is the highest of all 4 Clusters, 
#whereas Income mean is lower than in Cluster 2 and 1.

#BEST VALUE OF K 
mydata <- Data_clean
wss <- (nrow(mydata)-1)*sum(apply(mydata, 2, var))
for (i in 1:15)
  wss[i] <- sum(kmeans(mydata, centers = i, nstart = 100)$withinss)
plot(1:15, wss, type ="b", main = "Scree Plot", xlab = "# of Clusters", ylab = "Within Group SS", pch = 20, cex = 2)

#EVALUATE model -- Silhouette measure of the clusters obtained by best k 
#rerun model with k=8 clusters
set.seed(123)
kmModel_bestk <- kmeans(Data_clean, 8, nstart = 100)

# The number of instnces in each cluster is:
kmModel_bestk$size

# The cluster means, aka centroids, are:
kmModel_bestk$centers

# The variances within clusters are:
kmModel_bestk$withinss

# The variance between clusters is:
kmModel_bestk$betweenss

library(cluster)
ss <- silhouette(kmModel_bestk$cluster, dist(Data_clean))
mean(ss[ ,3])



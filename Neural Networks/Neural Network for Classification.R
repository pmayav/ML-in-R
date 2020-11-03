#Neural Network for Classification

#We'll use ISLR's built in College Data Set which has several features of a college and a 
#categorical column indicating whether or not the School is Public or Private.

library(ISLR)
data(College)
str(College) #all variables are numerical besides Private

#DATA PREPROCESSING
# Create vector of column Max and Min values
maxs = apply(College[ , 2:18], 2, max) 
# apply(x, margin, function). 
#If margin = 1, function is applied on the rows. If margin = 2, function is applied on the columns
mins = apply(College[ , 2:18], 2, min)
#normalizing data
scaled.data = as.data.frame(scale(College[ , 2:18], center = mins, scale = maxs - mins))

#SPLIT DATA
set.seed(1234)
ind = sample(2, nrow(College), replace = T, prob = c(0.7, 0.3))
TrainData = College[ind == 1, ]
TestData = College[ind == 2, ]

library(nnet)
nn = nnet(Private ~ ., data=TrainData, linout=F, size=10, decay=0.01, maxit=1000)
#The weights will be learned with a weight updating rate of 0.01 (the parameter decay).
#The parameter linout (linear out-put) indicates that the target variable is continuous or not. 
#The maxit parameter sets the maximum number of iterations of the weight convergence algorithm. 
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)

summary(nn)
# You could also use wts to get the best weights found and fitted.values to get the fitted
#values on training data
nn$wts
#nn$fitted.values

nn.preds = predict(nn, TestData, type = "class")
nn.preds

#CONFUSION MATRIX
table(TestData$Private, nn.preds)

#Accuracy
(55+157)/238




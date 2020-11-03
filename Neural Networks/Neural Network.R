#Neural Network Model
# we are predicting sales of child car seats at 400 different stores.

library(ISLR)
data("Carseats")
attach(Carseats)
data<- Carseats
#?Carseats

# ** NORMALIZE DATASET
#we can only normalize numerical variables
num_cols <- unlist(lapply(data, is.numeric))
num_cols
dataNum <- data[,num_cols]  #only numerical variables
str(dataNum)

#to normalize numerical variables we will use min - max transformation
#x in [min, max]
#x'= x-min / max -min, so x' in [0,1]

#min & max of all columns
mins<- apply(dataNum, 2, min) #1-rows, 2 -columns
maxs<- apply(dataNum, 2, max)

scaled.data<-as.data.frame(scale(dataNum, center = mins, scale= maxs - mins))
summary(scaled.data) #we can see now min is 0 and max is 1. 

#add to scaled df the variables that were factors (categorical)
data <- data.frame(scaled.data, data[!num_cols] )
str(data)

# ** CONSTRUCT NN MODEL
set.seed(123)
indx<- sample(2, nrow(data), replace=T, prob=c(0.8,0.2))
train <- data[indx==1, ]
test <- data[indx==2, ]

#library(neuralnet) 
library(nnet) 
nn <- nnet(Sales ~ . , data= train, linout= T , size= 10, decay=0.01) 
#linout -- stands for linear out-put and is used to determine whether the target variable is continuous or not
#linout = T if numerical varible, F if categorical variables. 
#size = X , sets the num of neurons in hidden layer
#decay = ,  regularization term [0,1] -> if small: large network-may overfit, if large: smaller network
summary(nn)
#b- bias, i-input,  h1-first neuron in hidden layer, o-output layer

#VISUALIZE FUNCTION
#To plot the neural network using nnet we need to use devtools
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)
#The darker lines are associated to the higher weights and gray lines are for small weights
#remember data was normalized

#nn$wts             #weights
#nn$fitted.values   #output for the training examples
#nn$residuals        #residuals

#TEST 
nn.preds<- predict(nn, test)
nn.preds   #predicted values on test data

#EVALUATE
#MEAN SQUARED ERROR
mse <- mean((nn.preds - test$Sales)^2)







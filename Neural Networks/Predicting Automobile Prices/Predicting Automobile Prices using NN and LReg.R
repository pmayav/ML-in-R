#Predicting Automobile Prices using Neural Networks

#We want to predict an accurate manufacturer's suggested retail price (MSRP) by using data collected for a previous batch of cars.
#This data contains 27 independent variables such as price, age, fuel type, horse power, etc, and 1 dependent variable (price of each car)

library(readxl)
data <- read_excel("~/Downloads/Car_Data.xlsx")
str(data)
#Data  Preprocessing
data$Fuel <- as.factor(data$Fuel)
data$MC <- as.factor(data$MC)
data$Colour <- as.factor(data$Colour)
data$Auto <- as.factor(data$Auto)
data$Mfr_G <- as.factor(data$Mfr_G)
data$ABS <- as.factor(data$ABS)
data$Abag_1 <- as.factor(data$Abag_1)
data$Abag_2 <- as.factor(data$Abag_2)
data$AC <- as.factor(data$AC)
data$Comp <- as.factor(data$Comp)
data$CD <- as.factor(data$CD)
data$Clock <- as.factor(data$Clock)
data$Pw <- as.factor(data$Pw)
data$PStr <- as.factor(data$PStr)
data$Radio <- as.factor(data$Radio)
data$SpM <- as.factor(data$SpM)
data$M_Rim <- as.factor(data$M_Rim)
data$Tow_Bar <- as.factor(data$Tow_Bar)
str(data)

#normalize numerical variables
#x'= x - min / max -min
num_cols <- unlist(lapply(data, is.numeric))
num_cols
dataNum <- data[,num_cols]  #only numerical variables
#min & max of all columns
mins<- apply(dataNum, 2, min) #1-rows, 2 -columns
maxs<- apply(dataNum, 2, max)

scaled.data<-as.data.frame(scale(dataNum, center = mins, scale= maxs - mins))
summary(scaled.data) #we can see now min is 0 and max is 1. 

#add to scaled df the variables that were factors (categorical)
data <- data.frame(scaled.data, data[!num_cols] )
str(data)

#we leave only categorical variables with more than 1 factor level and remove num variables that have the same value for all instances
data<- data[,-6] #remove drs
data<- data[,-6] #remove cyl
data<- data[,-9] #remove fuel
data<- data[,-13] #remove abs
data<- data[,-13] #remove abag_1
data<- data[,-19] #remove PStr
str(data)

# ** CONSTRUCT NN MODEL
set.seed(154)
indx<- sample(2, nrow(data), replace=T, prob=c(0.8,0.2))
train <- data[indx==1, ]
test <- data[indx==2, ]
 
library(nnet) 
nn <- nnet(Price ~ . , data= train, linout= T , size= 10, decay=0.01) 
summary(nn)

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn)

#TEST 
nn.preds<- predict(nn, test)
nn.preds   #predicted values on test data
  
#EVALUATE
#MEAN SQUARED ERROR
mse <- mean((nn.preds - test$Price)^2)  
mse


#************KFOLD
k <- 10
nmethod <- 1
folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE) 
model.meansquarederror <- matrix(-1, k, nmethod, dimnames=list(paste0("Fold", 1:k), c("NNet")))

for(i in 1:k)
{ 
  testindexes <- which(folds == i, arr.ind=TRUE) 
  test <- data[testindexes, ] 
  train <- data[-testindexes, ] 
  
  nnModel<- nnet(Price ~ . , data= train, linout= T, size= 10, decay=0.01)
  predicted <- predict(nnModel, test)
  model.meansquarederror[i] <- mean((test$Price - predicted)^2)
}

model.meansquarederror
mean(model.meansquarederror)
#*************

#NOW WE WILL USE **MORE** HIDDEN NEURONS
nn2 <- nnet(Price ~ . , data= train, linout= T , size= 17, decay=0.01) 

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn2)

#TEST 
nn.preds2<- predict(nn2, test)
nn.preds2   #predicted values on test data

#EVALUATE
#MEAN SQUARED ERROR
mse2 <- mean((nn.preds2 - test$Price)^2)  
mse2

#************KFOLD neurons=17
k <- 10
nmethod <- 1
folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE) 
model.meansquarederror <- matrix(-1, k, nmethod, dimnames=list(paste0("Fold", 1:k), c("NNet")))

for(i in 1:k)
{ 
  testindexes <- which(folds == i, arr.ind=TRUE) 
  test <- data[testindexes, ] 
  train <- data[-testindexes, ] 
  
  nnModel<- nnet(Price ~ . , data= train, linout= T, size= 17, decay=0.01)
  predicted <- predict(nnModel, test)
  model.meansquarederror[i] <- mean((test$Price - predicted)^2)
}

model.meansquarederror
mean(model.meansquarederror)
#*************

#NOW WE WILL USE **LESS** HIDDEN NEURONS
nn3 <- nnet(Price ~ . , data= train, linout= T , size= 5, decay=0.01) 

library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn3)

#TEST 
nn.preds3<- predict(nn3, test)
nn.preds3   #predicted values on test data

#EVALUATE
#MEAN SQUARED ERROR
mse3 <- mean((nn.preds3 - test$Price)^2)  
mse3

#************KFOLD neurons=5
k <- 10
nmethod <- 1
folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE) 
model.meansquarederror <- matrix(-1, k, nmethod, dimnames=list(paste0("Fold", 1:k), c("NNet")))

for(i in 1:k)
{ 
  testindexes <- which(folds == i, arr.ind=TRUE) 
  test <- data[testindexes, ] 
  train <- data[-testindexes, ] 
  
  nnModel<- nnet(Price ~ . , data= train, linout= T, size= 5, decay=0.01)
  predicted <- predict(nnModel, test)
  model.meansquarederror[i] <- mean((test$Price - predicted)^2)
}

model.meansquarederror
mean(model.meansquarederror)
#*************

#********** LINEAR REGRESSION 
set.seed(3)
indx<- sample(2, nrow(data), replace=T, prob=c(0.8,0.2))
train <- data[indx==1, ]
test <- data[indx==2, ]

lmModel <- lm(Price ~ . , data=train)
summary(lmModel)  

predictions <- predict(lmModel, test)
predictions
LMmse <- mean((predictions - test$Price)^2)  
LMmse

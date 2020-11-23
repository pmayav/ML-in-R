#KNN CLASSIFIER
library(ISLR)
data("Carseats")
Data<- Carseats
attach(Data) 
head(Data)

#create categorical variable for Sales (High, not high)
Data$Sales <- as.factor(ifelse(Sales>=8, "High", "Low"))

#When we use KNN data should be balanced, normalized, and all categorical variables 
#should be converted to numerical variables using dummy variables 

#checking if data is balanced
summary(Data$Sales)
mean(Data$Sales == "High") #41% is ok--> data is balanced, unbalanced if around 20%

#*** DATAFRAME 1
#normalizing numerical variables -- min max transformation
#if we don't  normalize, the variables with large values dominate the other variables in 
#the distance function 
num_vars <- unlist(lapply(Data, is.numeric))  
num_vars
DataNum<- Data[ , num_vars]  #new df of num vars only
head(DataNum)
    #min max normalization using scale function
mins <- apply(DataNum, 2 ,min)  #Gets min value of each column -- 1 rows, 2 cols
maxs <- apply(DataNum, 2, max)  #Gets max value of each column
Data.scaled <- scale(DataNum, center = mins, scale = maxs-mins)
summary(Data.scaled)

#*** DATAFRAME 2
#Convert categoricalc(factor) variables into numerical dummy variables
factor <- !num_vars  
factor
factor[1] <- "FALSE"    #this is the target variable, we want to keep it as factor
factor <- as.logical(factor)
factor
library(psych)

DataFactor <- as.data.frame(sapply(Data[, factor], dummy.code) ) 
head(DataFactor)      #df of dummy variables

#*** COMBINING 2 DATAFRAMES & target variables
Data_norm <- data.frame(Data.scaled, DataFactor, Data$Sales)
head(Data_norm)

#Split train/test 
set.seed(2)
indx  <- sample(2,nrow(Data_norm), replace=TRUE, prob = c(0.7, 0.3))
#we have to separate target variable from input variables (-15)
train <- Data_norm[indx==1, -15]  #removing target variable-Sales
test  <- Data_norm[indx==2, -15]  #removing target variable-Sales
ncol(train)

#vector of target variable
trainLabels <- Data_norm[indx==1, 15]
testLabels <- Data_norm[indx==2, 15]

#*** KNN MODEL
library(FNN)
    #knn() for classification, knn.reg() regression where target is numerical
    #knn(train= , test= , cl= , k= ) , where cl is class label (trainLabels)
    #knn.reg(train= , test= , y= , k= ) , check we use "y" here
#Given a point in the test data, the knn model computes the distance of the test point
#to all the training examples. Then uses the k NN to predict the label of the test point. 
#We are trying to predict testLabels

pred_class <- knn(train= train, test= test, cl=trainLabels, k=3, prob=T)
#first result is predicted classes 
#second result is the probability of predicted classes "prob", 
    #how many neighbors agree on the predicted label·
    #1.0000000- in this case, all 3 neighbors agree
    #0.6666667- 2 out of 3 are in the same class and 1 in diff. class
#third result- index of k (3) NN to all examples in test data
#fourth result- distance of each test example to k (3) NN

indices <- attr(pred_class, "nn.index")
print(indices[20,])  #returns 3 NN training examples of test point 20

probs <- attr(pred_class, "prob")
probs

dist <- attr(pred_class, "nn.dist")
print(dist[20, ])

table(pred_class, testLabels)
#not the best performance, change K-value using cross validation. 

#USING CROSS VALIDATION TO CHOOSE BEST K VALUE 
#partitioning taining data
indx<- sample(2, nrow(train), replace=TRUE, prob=c(0.6, 0.4))
trainD <- train[indx==1,]
validD<- train[indx==2,]

train_Labels<- trainLabels[indx==1]
valid_Labels<- trainLabels[indx==2]

ksize <- c()          #empty vector
accuracy <- c()
for (i in 1:15)
{
library(FNN)
prc_test_pred <- knn(train=trainD, test=validD, cl=train_Labels, k=i)
ksize <- c(ksize, i)
confusion_matrix <- table(valid_Labels, prc_test_pred)
accuracy <- c(accuracy, sum(diag(confusion_matrix))/sum(confusion_matrix))
}
result <- data.frame(ksize, accuracy)
result
plot(result)
#we can see best result here is k=7

#****RE-RUN KNN model with best value of k found on CV using the entire dataset.
pred_class_bestk <- knn(train= train, test= test, cl=trainLabels, k=7, prob=T)
table(pred_class_bestk, testLabels)

#accuracy of model improved. 

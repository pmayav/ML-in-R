#NAIVE BAYES CLASSIFIER
library(ISLR)
attach(Carseats)
head(Carseats) 

#create categorical variable for Sales (High, not high)
High<- as.factor(ifelse(Sales>=8, "YES", "NO"))
Carseats <- data.frame(Carseats,High)
Carseats <- Carseats[,-1] #delete first column (Sales col)

#Split train/test 
set.seed(2)
indx  <- sample(2,nrow(Carseats), replace=TRUE, prob = c(0.7, 0.3))
train <- Carseats[indx==1, ]
test  <- Carseats[indx==2, ]

#package for naive bayes model
#model 1
library(e1071)
naive_model <- naiveBayes(High ~ ., data= train)
naive_model

#for each variable we get a table of conditional probabilities
#categorical variable--P(Y given X)
#numerical variable-- 1st column average
#                      2nd column StDev
#we use these conditional probabilities for future prediction
pred_class <- predict(naive_model, test, type="class")
pred_class

#confusion matrix
table(pred_class, test$High, dnn= c("Prediction", "Actual"))

#accuracy 
(63+34)/(63+34+12+11)  #81% accuracy

#predicted probabilities
pred_prob <- predict(naive_model, test, type="raw") #raw
pred_prob


#model 2 - using laplace estimator
naive_model_laplace <- naiveBayes(High ~ ., data= train, laplace =1)
  #we add 1 instance to each of the categorical variables
naive_model_laplace

pred_class_laplace <- predict(naive_model_laplace, test, type="class")
pred_class_laplace

#confusion matrix
table(pred_class_laplace, test$High, dnn= c("Prediction", "Actual"))

#accuracy 
(64+34)/(64+34+12+10)  #81.66% accuracy
#accuracy only improves much when we have a zero frequency case.

naive_model_laplace$apriori
#individual conditional probability table
naive_model_laplace$tables$CompPrice 
#target variables
naive_model_laplace$levels

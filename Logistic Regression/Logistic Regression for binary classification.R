#LOGISTIC REGRESSION for binary classification

library(ISLR)
data("Carseats") 
attach(Carseats) 
str(Carseats)

set.seed(256)
#create new categorial variable
High <- as.factor(ifelse(Sales >= 8, "YES", "NO")) #categorical variable w/ 2 levels
Data <- data.frame(Carseats, High) #new df with High variable included
Data <- Data[ ,-1] #removes 1st column "Sales"
colnames(Data)[11] <- "Target" #change name to last (11th) column to Target
head(Data)

indx <- sample(2,nrow(Data), replace=T, prob = c(0.8, 0.2)) 
train <- Data[indx ==1, ]
test <- Data[indx ==2, ]

#glm - generalized linear model (~)
#glm(categorical target ~ inputs, data= train, family= "binomial")
logitModel <- glm(Target ~ . , data = train, family = "binomial")
summary(logitModel)

#Deviance: measure of goodness of fit of a glm : -2 log (likelihood) 
  #higher number - worse fit
#Null deviance: deviance of model with NO input variables, only intercept 
#Residual deviance: deviance of full model.

predictions <- predict(logitModel, newdata = test)
#predicted log of odds

predictions <- predict(logitModel, newdata = test, type="response")
#***** probability of being in class YES

Class <- ifelse(predictions >= 0.5, "YES", "NO") 
Class

test$Target == Class
#confusion matrix 
table(test$Target, Class)


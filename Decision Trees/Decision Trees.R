library(MASS)
library(plyr)
library(dplyr)
library(tibble)
library(ggplot2)
library(knitr)
library(gdata)
library(ISLR)

data("Carseats")
attach(Carseats)
#create new categorical variables "High"
High <- ifelse(Sales >= 8, "YES", "NO" )
High <- as.factor(High)

#Attach new variable to df & remove 1st column (Sales) of df 
Carseats <- data.frame(Carseats, High)
Carseats <- Carseats[-1]

#Divide data into train and test 
set.seed(3)
indx <- sample(2, nrow(Carseats), replace=T, prob= c(0.7, 0.3))
train <- Carseats[indx == 1, ]
test <- Carseats[indx ==2, ]

#most common package for decision trees
#this function uses gini/information gain for classfication prob
#install.packages("rpart")
library(rpart)

#TRAIN
#simplest model- using all (.) other variables as input variables
tree_model <- rpart(High ~ . , data=train)

#To visualize tree model
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree_model) 

print(tree_model)
#READS AS: node), split, n, loss, yval, (yprob)
#split - variabel name & condition , n - number of instances on the node, loss - instances predicted in wrong class, 
#yval - predicted class, (yprob) - probablitity in being in each class-> 
#(yprob) (x,y) first num x corresponds to the class predicted in first node
# * are terminal nodes 

#PREDICTION ON TEST DATA
tree_pred_probability <- predict(tree_model, test)
print(tree_pred_probability) #shows prob(confidence) of being in class 1 or 2

tree_pred_class <- predict(tree_model, test, type = "class")
print(tree_pred_class) #shows predicted class


#ACCURACY OF TEST DATA
#compares actual values == predicted 
mean(test$High == tree_pred_class)

#ACCURACY ON TRAIN DATA (==)
tree_pred_class_train <- predict(tree_model, train, type = "class")
mean(train$High == tree_pred_class_train)
#ERROR RATE ON TRAINING (!=)
mean(train$High != tree_pred_class_train)

#rpart(formula, data=train, parms= , control= )
#control-> controls how to split. control = rpart.control(minsplit=10)
#minsplit=10 -> at least 10 instances must be in each node 
#minbucket=10 -> min num of instances expected in terminal nodes
#cp -> complexity parameter -> want the one with min error & also size of tree 
#when cp is large - size of tree is small and error is larger
#when cp is small - size of tree is large and error is smaller

#__________________________________________________________________________________________________________________________________
#TRAIN NEW MODEL - *FULL TREE*
tree_model_full <- rpart(High ~ . , data=train, parms = list(split="information"), control = rpart.control(minsplit = 0, minbucket = 0, cp = -1))
rpart.plot(tree_model_full)

#PREDICTION ON TEST DATA *FULL TREE*
tree_pred_probability_full <- predict(tree_model_full, test)
print(tree_pred_probability_full) #shows prob(confidence) of being in class 1 or 2

tree_pred_class_full <- predict(tree_model_full, test, type = "class")
print(tree_pred_class_full) #shows predicted class

#ACCURACY ON TRAIN DATA (==) *FULL TREE*
tree_pred_class_train_full <- predict(tree_model_full, train, type = "class")
mean(train$High == tree_pred_class_train_full)
#ERROR RATE ON TRAINING (!=) *FULL TREE*
mean(train$High != tree_pred_class_train_full)

#ACCURACY OF TEST DATA *FULL TREE*
#compares actual values == predicted 
mean(test$High == tree_pred_class_full)

#_____________________________________________________________________
summary(tree_model)
#important to look at CP- xerror, the best CP value to use is the one with smallest xerror




# Extreme Gradient Boosting (xgb or xgboost) 

# classification of flowers using lenght of sepal, width of sepal, lenght of petal, and width of petal
# xgboost requires the target variable classes to be in an integer format starting with 0
library(xgboost)

data("iris")
attach(iris)
str(iris)
head(iris)

species <- iris$Species
#converting target variable "species" to integer format 
label <- as.integer(iris$Species)
label
    #converting first class to 0 , 2nd to 1, 3rd to 2
label <- as.integer(iris$Species) -1
label

#removing Species variable
iris$Species <- NULL

# * training data should not contain the target variable (label)

#SPLIT TRAIN/TEST
indx <- sample(2, nrow(iris), replace=TRUE, prob=c(0.75, 0.25))
train_data <- iris[indx==1, ]
train_labels <- label[indx==1]
test_data <- iris[indx==2, ]
test_labels <- label[indx==2]

  #if we had categorical variables, we would need to convert them to numerical using dummy variables

#transform data into "xgb.DMatrix" format
  #but the inputs to xgb.DMatrix have to be matrices themselves
train_data <- as.matrix(iris[indx==1, ])
test_data <- as.matrix(iris[indx==2, ])

# xgboost requires this input fotmat
xgb.train <- xgb.DMatrix(data = train_data, label=train_labels) 
xgb.test <- xgb.DMatrix(data = test_data, label=test_labels) 

# 2 functions to train model: xgb.train & xgboost 
params = list(booster="gbtree", max_depth = 5, 
              eta = .001, objective= "multi:softprob", 
              eval_metric= "mlogloss", num_class = length(levels(species)))

# nrounds = max num of iterations required in Grad Boosting Model  
  #(remember we are adding weak learners to improve model)
# early_stopping_rounds = performance of model in a validation data and 
#stops training once the performance on the validation data has not improved 
#after a certain number of iterations 
    #early_stopping_rounds = 10 --> (prevent overfitting)
      #if performance does not improve after 10 iterations, we stop.

#booster="gbtree" or "gblinear" 
#eta -- learning rate in gradient boosting 
      #F_M = F_(M-1) + eta Delta_M
# objective= "multi:softprob" - multiclass classification probs
              #reg:linear - for linear regression probs
              #binary:logistic - binary classification
              #multi:softmax - predict classes
              #multi:softprob - predict probabilities
#eval_metric=  mae, auc, error, logloss (for binary classification), mlogloss (multiclass)       

xgb.fit <- xgb.train(data = xgb.train, params = params, nrounds=1000, 
          watchlist = list(val1 = xgb.train, val2 = xgb.test), early_stopping_rounds= 10)


xgb.pred <- predict(xgb.fit , test_data, reshape=T)
head(xgb.pred) #we get predicted probabilities of each class

#formatting results
xgb.pred<- as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(species)
head(xgb.pred)


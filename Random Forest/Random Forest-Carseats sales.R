library(ISLR)
data("Carseats")
Data<- Carseats[sample(nrow(Carseats)),] 
attach(Data)

# We introduce the variable High to solve a classification problem as opposed to a regression problem
High <- as.factor(ifelse(Sales >= 8, "YES", "No"))
Data <- data.frame(Data,High)
Data <- Data[,-1]
colnames(Data)[11] <- "Target"

# Random Forest Model
library(randomForest)
rf <- randomForest(Target ~ ., data = Data, mtry = sqrt(ncol(Data) - 1), ntree = 300, proximity = T, importance = T)
rf
importance(rf, type = 2)
#rf$proximity
rf$predicted
#rf$votes

library(ROCR)
score <- rf$votes[, 2]
pred <- prediction(score, Data$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# How to find the best cut-off point in ROC curve
# The performance() function for ROC curve returns
# tpr, fpr and alpha-values (cut-off points). We need 
# to write a function that receives these information and 
# returns the best cut-off point
# Hence the input argument to the following function is perf

opt.cut <- function(perf){
  # mapply function applies the function FUN to all perf@x.values, perf@y.values,perf@alpha.values
  cut.ind <- mapply(FUN = function(x,y,p){d=(x-0)^2+(y-1)^2 # We compute the distance of all the points from the corner point [1,0]
  ind<- which(d==min(d)) # We find the index of the point that is closest to the corner
  c(recall = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]])},perf@x.values, perf@y.values,perf@alpha.values)
}

BestcutOffPoint <- opt.cut(perf)
BestcutOffPoint
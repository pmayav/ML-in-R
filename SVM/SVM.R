#SVM for binary classification 

#we will try to predict species of flowers (versicolor/ not versicolor)
data(iris)
str(iris)

iris$Speciesclass[iris$Species == "versicolor"] <- "TRUE"
iris$Speciesclass[iris$Species != "versicolor"] <- "FALSE"
iris$Speciesclass <- factor(iris$Speciesclass)
iris$Species <- NULL
View(iris)

set.seed(123)
indx<- sample(2, nrow(iris), replace=T, prob=c(0.8,0.2))
train <- iris[indx==1, ]
test <- iris[indx==2, ]

library(e1071)
svmModel<- svm(Speciesclass ~ . , data=train, type="C-classification" , cost = 100, kernel = "linear", gamma=1)
#C-classification - binary classification
#cost - cost of misclassification, if high-> not many misclassified points, margin can be small
       #if low -> make more mistakes, margn is larger
#to fin best cost and gamma values, use cross validation
svmModel

svmModel$coefs #returns alpha_i * y_i (y_i - is the label--> class 1 and class -1) 
#alphas of non SV are zero
svmModel$SV  #since we have 4 input variables, our SV are in 4 dimensions
svmModel$index  #what instances are SV

#classifier - w x + b
# w = sum ( alpha_i * y_i * x_i) in SV
w<- t(svmModel$coefs) %*% as.matrix(train[svmModel$index, 1:4])
w
b<- - svmModel$rho #negative of intercept
b
#w x + b, if >0 - assign to positive class, o.w - neg class

svmModel$decision.values  #predicted classes, assign given the sign








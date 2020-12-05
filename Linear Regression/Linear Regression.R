library(ISLR)
data("Carseats") 
attach(Carseats)

set.seed(123)
indx <- sample(2,nrow(Carseats), replace=T, prob = c(0.8, 0.2)) 
train <- Carseats[indx ==1, ]
test <- Carseats[indx ==2, ]

#lm - linear model (~)
#lm(num target ~ inputs, data= train)

lmModel <- lm(Sales ~ ., data= train)
summary(lmModel)

#estimate- coefficient of each variable in the linear model
#if coefficient is larger-> means variable is more important & sign means direction of relationship
#if we increase price, sales decrease.
#Pr(>|t|) -p value, whether there is a significant relationship between each variable & target variable
#we want r-squared to be large-> indicates good fit of model

#Population around the store does NOT affect sales much, also education level, if urban, and if store
#is in USA.

layout(matrix(c(1,2,3,4), 2, 2))
plot(lmModel)

#Residuals vs Fitted - we don't want to see any pattern, points should be 
  #scattered at random, should be a horizontal line.

#Scale-Location - variation of residuals, 
  #line should be horizontal-> means variation is the same around all points.
#Normal Q-Q - check that residuals have normal distribution, should be a 45 degree line.

#Residual vs Leverage - helps identify influential points. 
  #no influential points in our model.

#mean squared error
predictions <- predict(lmModel, newdata = test)

mean(test$Sales-predictions)^2 
## [1] 0.02526576

fitted(lmModel)	#predictions for train values

coefficients(lmModel)	#coefficients of regression

residuals(lmModel) #residuals from actual train values - predicted train values


library(ISLR)
data("Carseats") 
attach(Carseats) 

set.seed(256)
indx <- sample(2,nrow(Carseats), replace=T, prob = c(0.8, 0.2)) 
train <- Carseats[indx ==1, ]
test <- Carseats[indx ==2, ]

#****VARIABLE SELECTION****
#In general, we would like to reduce num of variables in our regression model.
#Eg: forward, backward, and stepwise selection.

#Extreme cases
full <- lm(Sales ~ . , data = train)
null <- lm(Sales ~ 1 , data = train) #only considers intercept, no variables as inputs

#**FORWARD SELECTION**
#Considers one variable at a time:
  #if it improves the model, we include the variable 
  #otherwise, we don't include variable.
#We can look at r-squared, f-test, or AIC.

step(null, scope = list(lower=null, upper =full), direction = "forward")
#check all possibilities from null case to full case

#Start:	AIC=704.82
  #Sales ~ 1
  #if I add X variable, see how AIC decreases 
  #we want the LOWEST AIC
    #we add ShelveLoc 
#Step:	AIC=566.68
  #Sales ~ ShelveLoc
    #now we add Price as it reduces AIC
#... repeat process until AIC is *NOT* reduced. 
#first row is <none>

#final model display at the end

#**BACKWARD ELIMINATION**
step(full, scope = list(lower=null, upper =full), direction = "backward")
#check all possibilities from full case to null case


#**STEPWISE ELIMINATION**
step(full, scope = list(lower=null, upper =full), direction = "both")

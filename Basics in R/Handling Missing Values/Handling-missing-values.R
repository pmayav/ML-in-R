#HANDLING MISSING VALUES

data <- read.csv("~/Downloads/hmeq.csv")
summary(data)

# Handling missing values of NUMERICAL variables

sum(is.na(data$NINQ)) # This give you the number of missing values in the variable NINQ

sum(complete.cases(data$NINQ)) # Count of complete cases in the variable NINQ

sum(!complete.cases(data$NINQ)) # Count of NOT complete cases in the variable NINQ

which(!complete.cases(data$NINQ)) # Which cases (row numbers) are NOT complete

# The function “na.omit()” DELETES ALL instances with missing values and returns
# the object with listwise deletion of missing values.

NINQ_Imputed = na.omit(data$NINQ) # Create new variable without missing values
sum(is.na(NINQ_Imputed))

# REPLACE missing values by a particular value (mean)
data$NINQ[is.na(data$NINQ)] = mean(data$NINQ, na.rm=TRUE) # Recode all NA in NINQ as the average value
sum(is.na(data$NINQ))

#  REPLACING using mice for looking at missing DATA PATTERN
library(mice)
md.pattern(data)
# The output tells us that 3551 samples are complete, 932 samples miss only DEBTINC, 158
# samples miss only the DEROG and so on.

# The “mice()” function takes care of imputing process.
NewData = mice(data, m=5, maxit=50, meth="pmm", seed=500)
summary(NewData)

    # m=5 refers to the number of imputed datasets. Five is the default value.
    # meth=’pmm’ refers to the imputation method. 
          # In this case we are using predictive mean matching as imputation method

# We can get back the completed dataset using the complete() function
New_data <- as.data.frame(complete(NewData, 1))
head(New_data)


#Handling missing values of CATEGORICAL variables: 

#As far as categorical variables are concerned, replacing categorical variables is usually not
#advisable. Some common practice include replacing missing categorical variables with the
#mode of the observed ones, however, it is questionable whether it is a good choice.

data$REASON <- as.factor(data$REASON)
data$JOB <- as.factor(data$JOB)

#remove NA values if only instances of categorical variables are missing
Data <- data[complete.cases(data), ]  #removes ALL rows where a value is missing


#we need to first deal with numerical missing values and then categorical variables. 
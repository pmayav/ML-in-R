#HANDLING OUTLIERS

data <- read.csv("~/Downloads/hmeq.csv")
summary(data)

# METHOD 1: 
#To detect the outliers, you can first draw the histogram to determine the range of outliers.

hist(data$CLAGE, main = "CLAG Variable Histogram", xlab = "CLAGE")

      #To remove the outliers we can use the “subset(DataSet name, Variable name < Bound)
DataNew  = subset(data, CLAGE < 500)
hist(DataNew$CLAGE, main = "CLAD Variable Histogram", xlab = "CLAGE")

# If you have more than one variable with outliers you can use the following formula:
# NewData = subset(Data name, Var1 name < Bound1 & Var2 name < Bound2 & · · ·)

# ---------------------------------------------------------------------------------------------------

# METHOD 2: 
#To detect the outliers, the command “boxplot.stats()$out” can be used which
#uses the Tukey’s method to identify the outliers ranged above and below the 1.5 × IQR.
boxplot(data$CLAG)
CLAG_OutLiers = boxplot.stats(data$CLAG)$out # We first save all the outliers in the vector
CLAG_OutLiers

Data<- data[-which(data$CLAG %in% CLAG_OutLiers),] #REMOVING OUTLIERS FROM DATA
boxplot(Data$CLAG)


boxplot(data$VALUE)
VALUE_OutLiers = boxplot.stats(data$VALUE)$out # We first save all the outliers in the vector
VALUE_OutLiers

Data<- data[-which(data$VALUE %in% VALUE_OutLiers),] #REMOVING OUTLIERS FROM DATA
boxplot(Data$VALUE)


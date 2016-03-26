#MIT Analytics Edge Week 2 - Forecasting Elantra Sales

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
elantra <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/elantra.csv')
library(ggplot2)

#Training set includes all observations for 2012 and earlier
elantra_train <- subset(elantra, Year <= 2012)
elantra_test <- subset(elantra, Year > 2012)

#Build linear regression model to predict monthly sales using Unemployment, CPI_all, CPI_energy, and Queries
#as the independent variables.
SalesLM = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = elantra_train)
summary(SalesLM)

#Modeling Seasonality
SalesLM2 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + Month, data = elantra_train)
summary(SalesLM2)
#This model is not better because the Adjusted R-Sq has gone down and none of the variables are
#very significant. 

#Given two monthly periods that are otherwise identical in the other variables in our model, what is the 
#absolute difference in predicted Elantra sales given that one period is in January and the other in March?
abs(110.69*1 - 110.69*3)
#January vs May?
abs(110.69*1 - 110.69*5)

#We've added Month as a numeric variable, but in reality Month should be a factor.
#By modeling Month as a factor variable, the effect of each calendar month is not restricted to be
#linear in the numerical coding of the month. 

#In the previous problem, we showed that for every month we move into the future our predicted sales goes up
#by 110.69. This isn't right because the effect of the month should not be affected by the numerical coding, 
#and by modeling month as a numeric variable, we cannot capture more complex effects. 

elantra_train$Monthf <- as.factor(elantra_train$Month)
elantra_test$Monthf <- as.factor(elantra_test$Month)
SalesLM3 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + Monthf, data = elantra_train)
summary(SalesLM3)

#Multicollinearity
#Changes in coefficient signs and signs that are counter to our intuition may be due to a multicollinearity 
#problem. To check, compute the correlations of the variables in the training set. 

#Correlations need to be with numeric variables
cor(elantra_train[, 1:7])
#CPI_energy is highly correlated with CPI_all, Queries, and Unemployment
#Queries is highly correlated with CPI_energy, CPI_all, and Unemployment
#Based on these results there are many highly correlated variables; as a result the sign change of 
#Queries is likely to be due to multicollinearity.

#Simplify the model
#Perform backward variable selection until there are no insignificant variables or variables for which
#all of the factor levels are insignificant.

#Remove Queries
SalesLM4 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Monthf, data = elantra_train)
summary(SalesLM4)
#Now there are no variables that are insignificant. Note that Month has a few values that are insignificant, but
#we don't want to remove it because many values are very significant.

#Predict with test data
PredSales = predict(SalesLM4, newdata = elantra_test)
SSE = sum((PredSales - elantra_test$ElantraSales)^2)

#What would the baseline method predict for all observations in the test set? Remember that the baseline
#method we use predicts the average outcome of all observations in the training set.
baseline = mean(elantra_train$ElantraSales)

#The baseline method is used in R-Sq calculation (to compute SST) simply predicts the mean of ElantraSales
#in the training set for every observation (i.e. without regard to any of the independent variables)

#Test set R-Sq
SST = sum((baseline - elantra_test$ElantraSales)^2)
R2 = 1 - SSE/SST

#Largest absolute error we make in our test set predictions
max(abs(PredSales - elantra_test$ElantraSales))
#Which period is this?
match(max(abs(PredSales - elantra_test$ElantraSales)), abs(PredSales - elantra_test$ElantraSales)) #= 5
PredSales[5]

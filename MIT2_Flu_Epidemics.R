MIT Analytics Edge Week 2 - Flu Epidemics

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
FluTrain <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/FluTrain.csv')
FluTest <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/FluTest.csv')
summary(FluTrain)

#Which week corresponds to the highest percentage of ILI-related physician visits?
match(max(FluTrain$ILI), FluTrain$ILI) # = 303
FluTrain[303, ]

#Which week corresponds to the highest percentage of ILI-related query fraction?
match(max(FluTrain$Queries), FluTrain$Queries)

#Alternative methods
#subset(FluTrain, ILI == max(ILI))
#OR
#which.max(FluTrain$ILI) will give row number

ggplot(aes(x = ILI), data = FluTrain) +
  geom_histogram()

#When handling a skewed dependent variable, it is often useful to predict the logarithm of 
#the dependent variable instead of the dependent variable itself--this prevents the small number
#of unusually large or small observations from having an undue influence on the sum of squared errors
#of predictive models. In this problem, we will predict the natural log of ILI. 

ggplot(aes(x = Queries, y = log(ILI)), data = FluTrain) +
  geom_point()

FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

#For a single variable linear regression model, there is a direct relationship between the R-sq and the 
#correlation between the independent and the dependent varibles. 
#R-sq = Correlation^2
cor.test(log(FluTrain$ILI), FluTrain$Queries)

#Normally we would obtain test set predictions from the model FluTrend1 by: 
#PredTest1 = predict(FluTrend1, newdata = FluTest)
#However, the dependent variable in our model is log(ILI), so PredTest would contain predictions
#of log(ILI). We want predictions of ILI. 

PredTest1 = exp(predict(FluTrend1, newdata = FluTest))

#What is our estimate for the percentage of ILI-related physician visits for the week March 11, 2012?
which(FluTest$Week == "2012-03-11 - 2012-03-17") #= 11
PredTest1[11]

#Relative error between our estimate and the observed values for the week of March 11, 2012 i.e.
#how far off is the estimate from the observed value?
#(Observed ILI - Estimated ILI)/Observed ILI
(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]

SSE = sum((PredTest1 - FluTest$ILI)^2)
SST = sum((mean(FluTrain$ILI) - FluTest$ILI)^2)
R2 = 1 - SSE/SST
RMSE = sqrt(SSE/nrow(FluTest))

##Training a Time Series Model
#First, need to decide the amount of time to lag the observations. Since ILI is reported with a 1 or 2-week lag,
#a decision maker cannot rely on the previous week's ILI value to predict the curren week's value. Instead, 
#the decision maker will only have data available from 2 or more weeks ago. We will build a variable
#called ILILag2 that contains the ILI vlaue from 2 weeks before the current observation. 

#Load time series package "zoo"
library(zoo)

ILILag2 <- lag(zoo(FluTrain$ILI), -2, na.pad = TRUE)
#na.pad = add missing values to the first 2 weeks of our dataset where we can't compute the data from
#2 weeks earlier.
FluTrain$ILILag2 <- coredata(ILILag2)
summary(FluTrain)

ggplot(aes(y = log(ILI), x = log(ILILag2)), data = FluTrain) +
  geom_point()

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

ILILag2 <- lag(zoo(FluTest$ILI), -2, na.pad = TRUE)
FluTest$ILILag2 <- coredata(ILILag2)
summary(FluTest$ILILag2)

#Fill in the missing values for ILILag2 in FluTest
FluTest$ILILag2[1] <- FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] <- FluTrain$ILI[nrow(FluTrain)]

PredTest2 = exp(predict(FluTrend2, newdata = FluTest))

SSE = sum((PredTest2 - FluTest$ILI)^2)
SST = sum((mean(FluTrain$ILI) - FluTest$ILI)^2)
R2 = 1 - SSE/SST
RMSE = sqrt(SSE/nrow(FluTest))

#This problem used a simple time series model with a single lag term. ARIMA models are a more general
#form of the model we built, which can include multiple lag terms as well as more complicated combinations
#of previous values of the dependent variable. 
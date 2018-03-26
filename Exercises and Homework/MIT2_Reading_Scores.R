#MIT Analytics Edge Week 2 -  Reading Test Scores

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
pisaTrain <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/pisa2009train.csv')
pisaTest <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/pisa2009test.csv')

str(pisaTrain)
#average reading score of males
tapply(pisaTrain$readingScore, pisaTrain$male, mean)
summary(pisaTrain)

#Linear regression discards observations with missing data, so we will remove all such observations
#from the training and testing data sets. 

pisaTrain <- na.omit(pisaTrain)
pisaTest <- na.omit(pisaTest)

#To include unordered factors in a linear regression model, we define one level as the "reference level" and
#add a binary variable for each of the remaining levels. In this way a factor with n levels is replaced
#by n-1 binary variables. The reference level is typically selected to be the most frequently occurring
#level in the data set. 

#Set reference level for raceeth to "White"
pisaTrain$raceeth <- relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth <- relevel(pisaTest$raceeth, "White")

#The period is short for all other variables.
lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))

#Student A  grade 11, Student B grad 9
143.76633 + 29.54271*11 - (143.76633 + 29.54271*9) # = 59.09
#Since Student A and Student B have a difference in grade of 2, the model predicts that student A 
#has a reading score that is 2*29.54 = 59.09 larger.

#In this model raceethAsian coefficient = -4.11033. What is the meaning of this coefficient? 
#This is the predicted difference in reading score between an asian student and a white student who 
#is otherwise identical (all other variables are held the same).

predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)
SSE = sum((predTest - pisaTest$readingScore)^2)
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
R2 = 1 - SSE/SST
RMSE = sqrt(SSE/nrow(pisaTest)) # OR sqrt(mean((predTest - pisaTest$readingScore)^2))

#Baseline Prediction and Test-Set SSE
#What is the predicted score used in the baseline model?
baseline = mean(pisaTrain$readingScore)

#What is the sum of squared errors in the baseline model on the testing set?
SST
#SSE is the sum of squared errors of the test set. 
#SST is the sum of squared errors of the baseline.

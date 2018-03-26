#MIT Analytics Edge Week 3 - Framingham Heart Study

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
framingham <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/framingham.csv')

#Risk factors the variables that increase the chances of a disease. Identifying these risk factors
#is the key to successful prediction of Coronary Heart Disease (CHD)
str(framingham)

#Split data into training set and testing set
library(caTools)
set.seed(1000)
split <- sample.split(framingham$TenYearCHD, SplitRatio = .65)
#First argument is the outcome variable. 
#When we have more data like we do here, we can afford to put less data in the training set and more in
#the test set. This will increase our confidence in the ability of the model to extend to new data. You typically 
#want to put between 50% and 80% of data in training set.

train <- subset(framingham, split == TRUE)
test <- subset(framingham, split == FALSE)

framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

predictTest = predict(framinghamLog, type = "response", newdata = test)
table(test$TenYearCHD, predictTest > .5)
#With a threshold of .5, we predict an outcome of 1, the true column, very rarely. This means that our model
#rarely predicts a 10-year CHD risk above 50%
#Accuracy of the model = (1069 + 11) / 1273 = .848
#Need to compare this to the accuracy of a simple baseline method. Since the more frequent outcome in this case
#is 0, the baseline method would alwys predict 0 or no CHD. So the baseline method would get an accuracy of
#(1069 + 6) / 1273 = .844 where 768 + 5 is the total number of true negative cases.
#Our model barely beats the baseline in terms of accuracy. 
#Do we still have a valuable model by varying the threshold?
#Let's compute the out-of-sample AUC.
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
#We have an AUC value of 74% on our test set, which means that the model can differentiate between low risk
#and high risk patients pretty well. 

#Risk Model Validation
#So far, we've done internal validation. Our model was good at making predictions for Framinham patients. It's
#still unclear if the model generalizes to other populations.
#Now want external validation

#MIT Analytics Edge Week 4 - Supreme Court Forecasting

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
stevens <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/stevens.csv')
str(stevens)

#Split data
library(caTools)
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio = .70)
train <- subset(stevens, spl ==TRUE)
test <- subset(stevens, spl == FALSE)

#Build CART model
library(rpart)
library(rpart.plot)
#method = "class" ensures classification tree as opposed to a regression tree
StevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, 
                     method = "class", minbucket = 25)
prp(StevensTree)
#First split in the tree is whether or not the lower court decision is liberal. If it is liberal, then we move
#to the left. If the respondent is a criminal defendent, injured person, politician, state, or the United States,
#then we predict 0, or affirm. 
#The prp function abbreviates the values of the independent variables. If you're not sure what the abbreviations are
#you could create a table of the variable to see all the possible values. 
#prp will select the abbreviation so that they're uniquely identifiable.

#CART tree is a series of decision rules

predictCART = predict(StevensTree, newdata = test, type = "class")
#We need to give type = "class" when making predictions for our CART model if we want the majority class 
#predictions. This is like using a threshold of .5.

table(test$Reverse, predictCART)
#Accuracy about 66%
(41+71)/(41+36+22+71)

#If you were to build a logistic regression model, you would get an accuracy of .665 and a baseline that always
#predicts reverse, the most common outcome, has an accuracy of .547

#CART model significantly beats this baseline model and is competitive with the logistic regression model.
#CART model is also much more interpretable than Logistic Regression models.

#ROC Curve
library(ROCR)
predictROC = predict(StevensTree, newdata = test)
predictROC
#For each observation in the test set, it gives two numbers which can be thought of as the probability of 
#outcome 0 and the probability of outcome 1.
#Each test set observation is classified into a subset, or bucket, of our CART tree. These numbers give the 
#percentage of training set data in that subset with outcome 0 and the percentage of training set data in that
#subset with outcome 1. We'll use the second column as the probabilities to generate an ROC curve.

pred = prediction(predictROC[, 2], test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)
#AUC
as.numeric(performance(pred, "auc")@y.values)

#Change minbucket value
StevensTree2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, 
                     method = "class", minbucket = 5)
prp(StevensTree2)
StevensTree3 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, 
                      method = "class", minbucket = 100)
prp(StevensTree3)

#Random Forests
library(randomForest)
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data = train, nodesize = 25, ntree = 200)
#randomForest() does not have a method arguement. So when we want to do a classification problem, we need to 
#make sure outcome is a factor.
train$Reverse <- as.factor(train$Reverse)
test$Reverse <- as.factor(test$Reverse)
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data = train, nodesize = 25, ntree = 200)
#No warning message now, so our model is ready to make predictions
predictForest = predict(StevensForest, newdata = test)
table(test$Reverse, predictForest)
#Accuracy about 67% 
(40+74)/(40+37+19+74)
#Our random forest model improved accuracy a little bit over CART. Sometimes random forest will significantly
#improve accuracy over CART and other times you'll see an even smaller improvement than here over CART.
#Keep in mind random forests have a random component.

set.seed(100)
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data = train, nodesize = 25, ntree = 200)
predictForest = predict(StevensForest, newdata = test)
table(test$Reverse, predictForest)
#Accuracy about 68.8%
(43+74)/(43+34+19+74)

set.seed(200)
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data = train, nodesize = 25, ntree = 200)
predictForest = predict(StevensForest, newdata = test)
table(test$Reverse, predictForest)
#Accuracy about 70.6%
(44+76)/(33+17+44+76)


#The accuracy for a more stable dataset will not change very much, but a noisy datset can be significantly
#affected by random samples.

#Cross-Validation
library(caret)
library(e1071)
#Define how many folds we want
#We can do this using the trainControl function
numFolds <- trainControl(method = "cv", number = 10)
#Need to pick possible values for our cp
cpGrid <- expand.grid(.cp = seq(.01, .5, .01))
#Perform cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, 
      method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
#Output is a table describing cross-validation accuracy for different cp paramters.
#First column gives the cp parameter that was tested and the second column gives the cross validation
#accuracy for that cp value. cp = .19 this is the cp value we want to use in our CART model. 

StevensTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, 
                       method = "class", cp = .19)
PredictCV = predict(StevensTreeCV, newdata = test, type = "class")
table(test$Reverse, PredictCV)
#Accuracy about 72.4%
(59+64)/(59+18+29+64)

#Cross-validation helps us make sure we're selecting a good paramter value, and ofen this will significantly
#increase the accuracy.If we already happend to select a good paramter value then the accuracy might not have
#increased that much. By cross-validation, we can be sure we're selecting a smart paramter value.
prp(StevensTreeCV)

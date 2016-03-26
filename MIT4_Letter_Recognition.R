#MIT Analytics Edge Week 4 - Letter Recognition

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
letters <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/letters_ABPR.csv')
str(letters)
#This is a multiclass classification problem

#Warm up
#Predicting B or not
letters$isB <- as.factor(letters$letter == "B")

#Split data
library(caTools)
set.seed(1000)
split <- sample.split(letters$isB, SplitRatio = .5)
train <- subset(letters, split == TRUE)
test <- subset(letters, split == FALSE)

#Baseline method that always predicts the most frequent outcome
table(train$isB)
#Most frequent outcome is not B
#Accuracy
table(test$isB)
(1175)/(1175+383)

#Build classification tree to predict whether a letter is B or not. Use default settings for cp and minbucket.
library(rpart)
library(rpart.plot)

CARTb <- rpart(isB ~ . - letter, data = train, method = "class") 
prp(CARTb)
predb = predict(CARTb, newdata = test, type = "class")
table(test$isB, predb)
#Accuracy about 93.5%
(1118+340)/(1118+57+43+340)

#Build random forest model to predict whether the letter is a B or not  using the training set. Use all variables
#except letter as independent variables. Use default settings for ntree and nodesize. Right before building the model
#set seed to 1000.
set.seed(1000)
library(randomForest)
bForest <- randomForest(isB ~ . - letter, data = train)
predForest = predict(bForest, newdata = test)
table(test$isB, predForest)
#Accuracy about 98.8%
(1165+374)/(1165+374+10+9)

#Random forests tend to improve on CART models in terms of predictive accuracy


#Predicting letters A, B, P, R
letters$letter <- as.factor(letters$letter)
#Split again with new outcome variable
set.seed(2000)
spl <- sample.split(letters$letter, SplitRatio = .5)
Ltrain <- subset(letters, spl == TRUE)
LTest <- subset(letters, spl == FALSE)

#Baseline model predicts most frequent class of all the options
table(Ltrain$letter)
#Accuracy about 25.8%
(402)/(394+383+402+379)

#Build classification tree to predict letter
CARTl <- rpart(letter ~ . - isB, data = Ltrain, method = "class")
prp(CARTl)
predl = predict(CARTl, newdata = LTest, type = "class")
table(LTest$letter, predl)
#Accuracy about 87.9%
(348+318+363+340)/nrow(LTest)

#Build a random forest model on the training data
set.seed(1000)
lForest <- randomForest(letter ~ . - isB, data = Ltrain)
predlf = predict(lForest, newdata = LTest)
table(LTest$letter, predlf)
#Accuracy about 98%
(390+380+393+364)/nrow(LTest)

#This accuracy is significantly higher than for CART. While the accuracy of CART decreased significantly as we
#transitioned from the problem of predicting B/not B (relatively simple) to the problem of predicting 4 letters, 
#the accuracy of the forest decreased by a tiny amount.
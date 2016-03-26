#MIT Analytics Edge Week 4 - Earnings from Census Data

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
census <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/census.csv')
str(census)

#Build logistic regression model to predict whether an individual's earnings are above 50k

#Split
library(caTools)
set.seed(2000)
split <- sample.split(census$over50k, SplitRatio = .6)
train <- subset(census, split == TRUE)
test <- subset(census, split == FALSE)

#Model
over50kLog = glm(over50k ~ ., data = train, family = binomial)
summary(over50kLog)
#Accuracy about 85.5% 
pred = predict(over50kLog, newdata = test, type = "response")
table(test$over50k, pred >= .5)
(9051+1888)/(9051+662+1190+1888)

#Baseline Accuracy about 76%
#Model always predicts less than 50k
table(train$over50k)
table(test$over50k)
(9713)/(9713+3078)

#AUC about 90.6%
library(ROCR)
ROCRpred = prediction(pred, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

#So logistic model for this data gets high accuracy. Also the significances of the variables give us a way to gauge
#which variables are relevant for this prediction task. However, it is not immediately  which variables are more
#important than the others, especially due to the large number of factor variables in this problem.

#Classification Tree Model
library(rpart)
library(rpart.plot)
CART50 <- rpart(over50k ~ ., data = train, method = "class")
prp(CART50)

#Accuracy about 84.7%
pred50 = predict(CART50, newdata = test, type = "class")
table(test$over50k, pred50)
(9243+1596)/(9243+470+1482+1596)

#This highlights a very regular phenomenon when comparing CART with logistic regression. CART often performs a little
#worse than logistic regression in out-of-sample-accuracy, however as in the case here, the CART model is often
#much simpler to describe and understand.

#ROC Curve and AUC for CART Model
pred50C = predict(CART50, newdata = test)
predictROC50C = prediction(pred50C[, 2], test$over50k)
perf = performance(predictROC50C, "tpr", "fpr")
plot(perf)
as.numeric(performance(predictROC50C, "auc")@y.values) #.847
#The probabilites from the CART model only take a handful of values (five, one for each end bucket/leaf of the tree)
#The changes in the ROC curve correspond to setting the threshold to one of those values
#Note: the number of variables used in the model does not determine who the ROC curve looks
#Note: smoothness of ROC curve will generally depend on the number of data points
#Note: in logistic regression, the continuity of an independent variables means that you will have a large range of
#predicted class probabilities in your test set data. So you will see a large range of true and false postives as  
#you change the threshold for generating predictions. In CART, the continuity of the variable does not affect the
#continuity of the predicted class probabilities; for our tree, there are only 5 possible probability values.

#Random Forest Model
#Let's down-sample the training set due to computational expense
set.seed(1)
trainSmall <- train[sample(nrow(train), 2000), ]

#Model
set.seed(1)
library(randomForest)
RF <- randomForest(over50k ~ ., data = trainSmall)
predRF = predict(RF, newdata = test)
table(test$over50k, predRF)
#Accuracy about 83.5%
(9586+1093)/(9586+127+1985+1093)

#Random forests build large collection of trees and as a result lose some interpretability that comes with CART
#in terms of seeing how predictions are made and which variables are important. However, we can still compute metrics
#that give us insight into which variables are important.
#One metric: Number of times, aggregated over all trees in the forest, that a certain variable is selected for a 
#split. 
vu = varUsed(RF, count = TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(RF$forest$xlevels[vusorted$ix]))
#This is a chart that for each variables measures the number of times that variables was selected for splitting
#Age is used significantly more than other variables to split

#Second metric: impurity
#Measures how homogenous each bucket or leaf of the tree is. In each tree in the forest, whenever we select a 
#variable and perform a split, the impurity is decreased. So one way to measure the importance of a variable
#is to average the reduction in impurity, taken over all times the variable is selected for splitting in all of the 
#trees in the forest.
varImpPlot(RF)
#Note: the importance as measured by average reduction in impurity is in general different from the importance
#as measured from the number of splits for a variable. Although age and occupation are important in both metrics
#the order of the variables is not the same in the two plots. 

#How does CART behave with different choices of paramters?
#Let's select the cp paramter for our CART model using k-fold validation, with k = 10 folds. Do this by using the
#train function. Set the seed before hand to 2. Test cp values from .002 to .1 in .002 increments:
library(caret)
library(e1071)
set.seed(2)
numFolds <- trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(.002, .1, .002))
train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)
#In other words, the best value of cp is .002, corresponding to the lowest cp value. Often cp needs to become quite
#low before the accuracy begins to detiorate.

#Fit CART model with this cp value
TreeCV <- rpart(over50k ~ ., data = train, method = "class", cp = .002)
predCV = predict(TreeCV, newdata = test, type = "class")
table(test$over50k, predCV)
#Accuracy about 86.1%
(9178+1838)/(9178+535+1240+1838)

#Accuracy improved for this CART model vs the one with the default cp value, so should we favor it?
prp(TreeCV)

#This highlights one important tradeoff in building predictive models. By tuning cp we improved our accuracy by over
#1%, but our tree has become significantly more complicated. In some applications, such an improvement would be
#worth the loss of interpretability. In others, we may prefer a less accurate model that is simpler to understand
#and describe over a more accurate--but more complicated--model. 
#Here there are about 18 splits!

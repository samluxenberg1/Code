#MIT Analytics Edge Week 4 - State Data (Revisited)

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
data(state)
statedata = data.frame(state.x77)
#We will try to build a model for life expectancy using regression trees, and employ cross-validation to improve our
#trees performance.

#Linear Regression
linReg = lm(Life.Exp ~ ., data = statedata)
summary(linReg)
#SSE
predLM = predict(linReg)
SSE = sum((predLM - statedata$Life.Exp)^2)
SSE
#Or
sum(linReg$residuals^2)
linReg2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
summary(linReg2)
#SSE
sum(linReg2$residuals^2)
cor(statedata)

#Trying different combinations of variables in linear regression controls the complexity of the model. This is similar
#to trying different numbers of splits in a tree, which is also controlling the complexity of the model. 

#CART
#Regression Tree
library(rpart)
library(rpart.plot)
leTree = rpart(Life.Exp ~ ., data = statedata)
prp(leTree)
#The only variable used is Murder
#Use regression tree to predict life expectancies and calculate SSE
predle = predict(leTree)
SSE_T = sum((predle - statedata$Life.Exp)^2)
SSE_T #=29
#Error is higher than for linear regression models. One reason might be that we haven't made the tree big enough.
leTree = rpart(Life.Exp ~ ., data = statedata, minbucket = 5)
prp(leTree)
#Since the tree now has more splits, the default minbucket paramter was limiting the tree from splitting more. 
#The default minbucket paramter must be larger than 5. 
predle = predict(leTree)
SSE_T = sum((predle - statedata$Life.Exp)^2)
SSE_T #=29
#This is much closer to linear regression models' error. By changing the parameters we have improved the fit of our
#model.

#Can we do better?
le2Tree = rpart(Life.Exp ~ Area, data = statedata, minbucket = 1)
prp(le2Tree)
predle2 = predict(le2Tree)
SSE_T2 = sum((predle2 - statedata$Life.Exp)^2)
SSE_T2 # = 9.3
#By setting minbucket = 1, this does not necessarily mean that each bucket only has one observation. We're mearly
#allowing for a bucket to have 1 observation

#We can build almost perfect models given the right paramters, even if they violate our intuition of what a good 
#model should be. However as you see here there are 22 splits which is not very interpretable and doesn't 
#generalize well

#Cross-Validation
#Adjusting the variables in a linear regression model is a form of model tuning.
#We will tune our regression tree to see if we can improve the fit of our tree while keeping it as simple as possible.
library(caret)
set.seed(111)
#10 folds
fitControl <- trainControl(method = "cv", number = 10)
cGrid <- expand.grid(.cp = seq(.01, .5, .01))
train(Life.Exp ~ ., data = statedata, method = "rpart", trControl = fitControl, tuneGrid = cGrid)

#Create tree with this value of cp
leCVTree = rpart(Life.Exp ~ ., data = statedata, cp = .12)
prp(leCVTree)
#Here the life expectancy is predicted to be 70 if the Murder rate is between 6.6 and 11.
#SSE = 32.9
predleCV = predict(leCVTree)
sum((predleCV - statedata$Life.Exp)^2)

#out of the 3 tree models which would be the best at predicting on new observations (some test set)
#Model 1: 5 splits, SSE = 29, default paramters
#Model 2: 22 splits, SSE = 9.3, minbucket = 1
#Model 3: 2 splits, SSE = 32.9, Cross-Validation, cp = .12
#The purpose of cross-validation is to pick the tree that will perform the best on a test set. So we would expect
#the model we made with the best cp to perform best on the test set.

train(Life.Exp ~ Area, data = statedata, method = "rpart", trControl = fitControl, tuneGrid = cGrid)
leACVTree = rpart(Life.Exp ~ Area, data = statedata, cp = .03)
prp(leACVTree)
#The lower left leaf/bucket corresponds to the lowest predicted Life.Exp of 70. Observations in this leaf correspond
#to states with area greater or equal to 9579 and less than 51,000.

#We have simplified the previous "Area" tree considerably by using cross-validation. 
#Calculate SSE of the cross-validated area tree. = 44.27
preleACV = predict(leACVTree)
sum((preleACV - statedata$Life.Exp)^2)

#The Area variable is not as predictive as Murder rate. The original Area tree was over-fitting the data - it was 
#uninterpretable. Area is not as useful as Murder - if it was, it would have been in the cross-validated tree. 
#Cross-validation is not designed to improve the fit on the training data, but it won't necessarily make it worse 
#either. Cross-validation cannot guarantee improving the SSE on unseen data, although it often helps. 

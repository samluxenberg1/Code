#MIT Analytics Edge Week 6 - Stock Returns

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
stocks <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/StocksCluster.csv')
str(stocks)
summary(stocks)
head(stocks)
cor(stocks)

#Initial Logistic Regression Model
library(caTools)
set.seed(144)
spl <- sample.split(stocks$PositiveDec, SplitRatio = .7)
stocksTrain <- subset(stocks, spl == TRUE)
stocksTest <- subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec ~ ., data = stocksTrain, family = binomial)
summary(StocksModel)
predictTrain = predict(StocksModel, type = "response")
table(stocksTrain$PositiveDec, predictTrain >= .5)

#Accuracy on training data about 57.12%
(990+3640)/(990+2689+787+3640)
predictTest = predict(StocksModel, newdata = stocksTest, type ="response")
table(stocksTest$PositiveDec, predictTest >= .5)

#Accuracy on testing data about 56.71%
(417+1553)/(417+1160+344+1553)

#Accuracy of Baseline Method (always predicts PostiveDec = 1) about 54.61%
table(stocksTrain$PositiveDec)
table(stocksTest$PositiveDec)
1897/(1577+1897)

#Clustering Stocks
#Remove dependent variable
limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL
limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL

#Why do we need to remove the dependent variable in the clustering phase of the cluster-then-predict methodology?
#Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the
#methodology.

#In cluster-then-predict, our final goal is to predict the dependent variable, which is unknown to us at the time 
#of prediction. Therefore if we need to know the outcome value to perform the clustering, the methodology is no 
#longer useful for prediction of an unknkown outcome variable. 

#In cases where we have a training and testing set, we'll want to normalize by the mean and standard deviation
#of the variables in the training set. 
library(caret)
preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)

mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

#If they were both normalized, why is the mean of the ReturnJan variable much closer to 0 in normTram than in 
#normTest? 
#The distribution of the ReturnJan variable is different in the training and testing set.

set.seed(144)
km <- kmeans(normTrain, centers = 3, iter.max = 1000)

table(km$cluster)
#Or
km$size

#Use flexclust package to obtain training set and testing set cluster assignments for our observations.
library(flexclust)
km.kcca <- as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)
str(clusterTest)
table(clusterTest)

#Cluster-Specific Predictions
stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)

stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

StocksModel1 = glm(PositiveDec ~ ., data = stocksTrain1, family = binomial)
StocksModel2 = glm(PositiveDec ~ ., data = stocksTrain2, family = binomial)
StocksModel3 = glm(PositiveDec ~ ., data = stocksTrain3, family = binomial)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

#Test set predictions
PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type = "response")

#Accuracy for Cluster 1 about 61.94%
table(stocksTest1$PositiveDec, PredictTest1 >= .5)
(30+774)/(30+471+23+774)

#Accuracy for Cluster 2 about 55.05%
table(stocksTest2$PositiveDec, PredictTest2 >= .5)
(388+757)/(388+626+309+757)

#Accuracy for Cluster 3 about 64.58%
table(stocksTest3$PositiveDec, PredictTest3 >= .5)
(49+13)/(49+13+13+21)

#To compute the overall accuracy of the cluster-then-predict approach, we can combine all the test set predictions
#into a single vector and all the true outcomes into a single vector.
AllPredictions <- c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes <- c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions >= .5)
(467+1544)/(467+1110+353+1544)

#So the overall accuracy of this methodology is about 57.89%.

#We see a modest improvement over the original logistic regression model. Since predicting stock returns is a 
#notoriously hard problem, this is a good increase in accuracy. By investing in stocks for which we are more 
#confident that they will have positive returns (by selecting the ones with higher predicted probabilities),
#this cluster-then-predict model can give us an edge over the original logistic regression model.
#MIT Analytics Edge Week 4 - Claims Data

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
Claims <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/ClaimsData.csv')
str(Claims)
#Percentage of patients in each bucket
table(Claims$bucket2009)/nrow(Claims)
#Goal: Predict the cost bucket the patient fell into in 2009

#Split data
library(caTools)
set.seed(88)
spl <- sample.split(Claims$bucket2009, SplitRatio = .6)
claimsTrain <- subset(Claims, spl == TRUE)
claimsTest <- subset(Claims, spl == FALSE)

summary(claimsTrain$age)
table(claimsTrain$diabetes)/nrow(claimsTrain)

#Baseline Method and Penalty Matrix
#Baseline method would predict that for the cost bucket for a patient in 20009 will be the same as it was in 2008.
table(claimsTest$bucket2009, claimsTest$bucket2008)
#Baseline Accuracy about 68.4%
(110138+10721+2774+1539+104)/nrow(claimsTest)

#Penalty Error
#First compute Penlty Matrix
#Actual outcomes on the left and the predicted outcomes on the top
PenaltyMatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)
PenaltyMatrix
#Compute penalty error by the baseline method
#Multiply our classification matrix by our penalty matrix
as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008))*PenaltyMatrix
#This isn't typical matrix multiplication. Here each entry in classification matrix is multiplied by the
#corresponding entry in the penalty matrix.

#Penalty Error of baseline method is about .74
#Sum it up and divide by the number of observations in our test set. 
sum(as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008))*PenaltyMatrix)/nrow(claimsTest)

#Suppose the baseline method would just predict the most frequent out, cost bucket 1. 
#Compute accuracy and penalty error
table(claimsTest$bucket2009)
(122978)/nrow(claimsTest)
PenaltyMatrix2 <- matrix(c(0,2,4,6,8), byrow = TRUE, nrow = 5)
sum(as.matrix(table(claimsTest$bucket2009))*PenaltyMatrix2)/nrow(claimsTest)

#Predicting Health Care Costs (multi-class classification problem)
library(rpart)
library(rpart.plot)

claimsTree <- rpart(bucket2009 ~ age +arthritis + alzheimers + cancer + copd + depression + diabetes + 
                      heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008,
                    data = claimsTrain, method = "class", cp = .00005)
#Note the cp value was selected from cross-validation that would take too long to do now
prp(claimsTree)
PredictTest = predict(claimsTree, newdata = claimsTest, type = "class")
table(claimsTest$bucket2009, PredictTest)
#Accuracy about 71.3%
(114141+16102+118+201)/nrow(claimsTest)
#Penalty Error .758
sum(as.matrix(table(claimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(claimsTest)

#While we increased the accuracy, the penalty error also went up 
#Why?
#By default, rpart will try to maximize the overall accuracy, and every type of error is seen as having a 
#penalty of 1. Our CART model predicts 3, 4 and 5 so rarely because there are very few observations in these
#classes.So we don't really expect this model to do better on the penalty error than the baseline method.
#How to fix this?
#rpart() allows us to specify a paramter called loss. This is the penalty matrix we want to use when building 
#our model. 
claimsTree <- rpart(bucket2009 ~ age +arthritis + alzheimers + cancer + copd + depression + diabetes + 
                      heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008,
                    data = claimsTrain, method = "class", cp = .00005, parms = list(loss = PenaltyMatrix))
#If rpart knows that we'll be giving higher penalty to some types of errors over others, it might choose 
#different splits when building the model to minimize the worst types of errors. We'll probably get a lower
#overall accuracy with this new model, but hopefully the penalty error will be much lower too. 
PredictTest = predict(claimsTree, newdata = claimsTest, type = "class")
table(claimsTest$bucket2009, PredictTest)
#Accuracy about 64.7%
(94310+18942+4692+636+2)/nrow(claimsTest)
#Penalty Error about .642
sum(as.matrix(table(claimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(claimsTest)

#So our accuracy is lower than the baseline method but our penalty error is also much lower.   

#The first CART model, without loss matrix, predicted bucket 1 for 78.6% of the observations in the test set.
#Did the second CART model, with loss matrix, predict bucket 1 for more or fewer of observations and why?
colSums(table(claimsTest$bucket2009, PredictTest)) 
106515/nrow(claimsTest)
# = 58.1%
#According to the penalty matrix, some of the worst types of errors are to predict bucket 1 when the actual 
#cost bucket is higher. Therefore the model with the penalty matrix predicted bucket 1 less frequently. 
#MIT Analytics Edge Week 3 - Parole Violation

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
parole <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/parole.csv')
str(parole)
#How many violated the terms of their parole?
table(parole$violator)
table(parole$race)
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
summary(parole)

#Split into training and testing
set.seed(144)
library(caTools)
split <- sample.split(parole$violator, SplitRatio = .7)
train <- subset(parole, split == TRUE)
test <- subset(parole, split == FALSE)
#If you set a random seed, split, set the random seed again to the same value, and then split again, you will
#get the same split. However if you set the seed and then split twice, you will get different splits. If you set
#the seed to different values you will get different splits. 

#Build a Logistic Regression Model
paroleLog = glm(violator ~ ., data = train, family = binomial)
summary(paroleLog)
#If we have a coefficient, c, for a variable then that means that the log odds (or logit) are increased by c
#for a unit increase in the variable. This means the odds are multiplied by e^c for a unit increase in the variable.
#So since multiple offenses has coefficient 1.612, this means the log odds are increased by 1.612 for a 1 unit 
#increase in the variable. So the odds are multiplied by e^1.612 for a 1 unit increase in the variable. 
#Since e^1.612 = 5.013, the odds are 5.013 times higher for violating parole when there is a 1 unit 
#increase in multiple offenses.

#If parolees A and B are identical other than A having committed multiple offenses, the predicted log odds of A
#is 1.61 more than log odds of B. 
#ln(odds of A) = ln(odds of B) + 1.61
#exp(ln(odds of A)) = exp(ln(odds of B) + 1.61) = exp(ln(odds of B))*exp(1.61) = (odds of B)*5.01
#So, odds of A = 5.01*(odds of B)
#Our model predicts that a parolee who committed multiple offenses has 5.01 times higher odds of being a 
#violator than a parolee who did not commit multiple offenses but is otherwise identical.

#Say there is a parolee with the following characteristics:
#white, male, age 50, Maryland, served 3 months, max sentence 12 months, no multiple offenses, larceny
#What are the odds this individual is a violator?
exp(-4.2411574 + .3869904 + .8867192 + 50*(-.0001756) + 3*(-.1238867) + 12*(.0802954) + .68377143)
# = .1825
#What is the probability this individual is a violator?
1/(exp(-1*(-4.2411574 + .3869904 + .8867192 + 50*(-.0001756) + 3*(-.1238867) + 12*(.0802954) + .68377143))+1)
# = .1544

#Evaluate the model on the testing set
TestPred = predict(paroleLog, newdata = test, type = "response")
summary(TestPred)
#use threshold .5
table(test$violator, TestPred >= .5)
#Sensitivity (probability of predicting a "success" correctly, where a success is the individual is a violator)
12/(11+12)
#Specificity (prob. of predicting a "failure" correctly, where a failure is the individual is not a violator)
167/(167+12)
#Accuracy
(167+12)/(167+12+12+11)

#Accuracy of baseline model
#Where baseline model is predicting that every parolee is a non-violator
(167+12)/(167+12+12+11)

#Since false negatives are parolees who actually are violators but were predicted not to be violators, 
#the board assigns more cost to these people. So they would want to lower the cut off threshold.
#The probability of being predicted to be a violator is lower and thus capturing more of these violators. 
#This would increase the number of false positives and decrease the number of false negatives.

#What the the AUC value for the model? .8946
library(ROCR)
ROCRpred = prediction(TestPred, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

#Interpretation of AUC value
#Given a random parolee from the dataset who actually did violate his/her parole, and a random parolee
#who actually did not violate his/her parole, the AUC = 89.5% is the percentage of time that our model
#will classify which is which correctly. This is independent of the regression cutoff selected.

#Evaluate a potential source of bias
#The dataset contains all individuals released from parole in 2004, either due to completing their parole term
#or violating the terms of their parole. However, it does not contain parolees who neither violated their parole
#nor completed their term in 2004, causing non-violators to be underrepresented.This is called "selection bias"
#or "selecting on the dependent variable," because only a subset of all relevant parolees were included in our
#analysis, based on our dependent variable in this analysis (parole violation). How could we improve the
#dataset to best address selection bias?

#A prospective dataset that tracks a cohort of parolees and observes the true outcome of each is more
#desirable. These datasets are more difficult to obtain and might require tracking an individual for 10 years
#before building the model. This would not be possible using the 2004 dataset.

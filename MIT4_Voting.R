#MIT Analytics Edge Week 4 - Voting

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
gerber <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/gerber.csv')
str(gerber)
#Proportion of people who voted
table(gerber$voting)/nrow(gerber)
table(gerber$voting, gerber$hawthorne) #.3223
table(gerber$voting, gerber$neighbors) #.3779
table(gerber$voting, gerber$civicduty) #.3359
table(gerber$voting, gerber$self) #.3452
#Or
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$neighbors, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)

#Build logist regression model for voting using four treatment groups as independent variables
votingLog = glm(voting ~ civicduty + neighbors + hawthorne + self, data = gerber, family = binomial)
summary(votingLog)
#using threshold of .3 what is the accuracy of the logistic regression model?
pred = predict(votingLog, type = "response")
table(gerber$voting, pred >= .3)
(134513+51966)/(134513+100875+56730+51966)
#What is accuracy with threshold of .5?
table(gerber$voting, pred >= .5)
summary(pred)
(235388)/(235388+108696)

#Baseline Accuracy (percentage of people who did not vote)
(134513+100875)/(134513+100875+56730+51966)

#AUC
library(ROCR)
ROCRpred = prediction(pred, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

#Even though all the variables are significant, our model does not improve over the baseline model of just predicting
#that someone will not vote, and the AUC is low. So while the treatment groups do make a difference, this is 
#a weak predictive model.

#Trees
#We are interested in building a regression tree to explore the fraction of people who vote, or the probability
#of voting. We'd like CART to split our groups if they have different probabilities of voting.
#If we use method = "class", CART would only split if one of the groups had a probability of voting above 50% and
#the other had a probability of voting less than 50%. However, with regression trees, CART will split even if both
#groups have probability less than 50%.
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
prp(CARTmodel)
#There is only one leaf. There are no splits in the tree, because none of the variables make a big enough effect
#to be split on. 

#Let's force the complete tree to be built  
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, cp = 0)
prp(CARTmodel2)
#We saw before that the highest fraction of voters was in the neighbors group, followed by the self group, followed
#by the Hawthorne group, and lastly by the civic duty group. The tree detects this trend. 

#Based on the tree, what fraction of civic duty voted?
#The people in the civic duty group correspond to the bottom right split, which has value .31 in the leaf. 

#Make new tree with sex
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data = gerber, cp = 0)
prp(CARTmodel3)

#In the control group which gender is more likely to vote?
#There is a split on the sex variable after every treatment variable split. For the control group, which corresponds
#to the bottom left, sex = 0 (male) corresponds to higher voter percentage. Similarly for civic duty

#Interaction Terms
#Trees can handle nonlinear relationships e.g. in the Civic duty group and female, but it is also possible to do the
#same with logistic regression.
#Create a regression tree using just the control variable. 
CARTmodel4 = rpart(voting ~ control, data = gerber, cp = 0)
prp(CARTmodel4)

CARTmodel5 = rpart(voting ~ control + sex, data = gerber, cp = 0)
prp(CARTmodel5, digits = 6)

#In the control only tree, what is the absolute value of the difference in the predicted probability of voting 
#between being in the control group vs being in a different group? 
prp(CARTmodel4, digits = 6)
abs(.296638-.34)
#The split says that if control = 1, predict .296638, and if the control = 0, predict .34.

#Using the second control tree, determine who is affected more by not being in the control group.
abs(.290456 - .334176) #=.04372 Woman being in the control group vs not being in control group
abs(.302795 - .345818) #=.043023 Men being in the control group vs not being in control group.
#both affected about the same within .001

votingLog2 = glm(voting ~ control + sex, data  = gerber, family = binomial)
summary(votingLog2)
#Negative coefficient for sex indicates that women are less likely to vote since sex = 1 for female.

#The regression tree calculated the percentage voting exactly every one of the 4 possibilities with the sex and 
#control variables. Logistic regression attempted to do the same, although it wasn't able to do as well because
#it can't consider exactly the joint possibility of being a woman and being in the control group.

#Let's qunatify this precisely. 
#Create the data frame which contains all possible values of sex and control, and evaluate your logistic 
#regression using the predict function.
possibilities <- data.frame(sex = c(0,0,1,1), control = c(0,1,0,1))
possibilities
predict(votingLog2, newdata = possibilities, type = "response")
#What is the absolute difference between the tree and the logistic regression for (Woman, Control) case?
abs(.290456 - .2908065)

#for this dataset the difference is not too big, but it is there.
#Let's add a new term to our logistic regression. This term will the combination of the sex and control variables.
#If this variable is 1, the person is a woman and is in the control group.
votingLog3 = glm(voting ~ control + sex + sex:control, data = gerber, family = binomial)
summary(votingLog3)
#Interpretation for the coefficient of the combination variable in isolation?
#If sex:control = 1, meaning the individual is female and in the control group, then she is less likely to vote.
possibilities <- data.frame(sex = c(0,0,1,1), control = c(0,1,0,1))
predict(votingLog3, newdata = possibilities, type = "response")
abs(.290456 - .2904558)
#Now there is very smally difference (practically 0) between the logistic regression and CART.

#This example has shown that trees can capture nonlinear relationships that logistic regression cannot, but that
#we can get around this sometimes by using variables that are the combination of the two variables. 
#Should we always include all possible interaction terms of the independent variables when building a logistic 
#regression model?
#This could cause over-fitting especially in smaller datasets

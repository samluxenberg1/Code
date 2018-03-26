#MIT Analytics Edge Week 3 - Logistic Regression with Healthcare Quality

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
quality <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/quality.csv')
str(quality)
table(quality$PoorCare)

#Before building any models, let's consider using a simple baseline method.
#For a classification problem, a standard baseline method is to just predict the most frequent outcome 
#for all observations. 
#Since 98 of 131 observations are receiving good care (about 75%), set our baseline model to this. 
#Our baseline model has an accuracy of 75% 
#We will try to beat this with our logistic regression model.

#We want to randomly split our data set into a training set and a testing set so that we'll have a test set
#to measure out-of-sample accuracy. 
library(caTools)

set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = .75)
#sample.split randomly splits the data, but it also makes sure the outcome variable is well-balanced
#in each piece. So 75% of the patients in both the testing and training sets receive good care. We do this
#because we want our test set to be representative of our training set.
split
#TRUE means we put that observation in training set and FALSE means we put that observation in testing set.
qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)

###If we wanted to instead split a data frame, data, where the dependent variable is continuous, you could
###instead use the sample() function. Here is how to select 70% of the observations for the training set
###and 30% for the test set. 
##spl <- sample(1:nrow(data), size = .7 * nrow(data))
##train <- data[spl, ]
##test <- data[-spl, ]


#Build Logistic Regression model using generalized linear model function (glm)
qualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
#family = binomial argument tells glm function to build logistic regression model
summary(qualityLog)
#coefficient estimates of both OfficeVisits and Narcotics are positive which mean that higher values of these
#two variables are indicative of poor care
#Both coefficients are significant by the stars
#AIC measures quality of the model and is like Adjusted R-Sq in that it accounts for the number of 
#variables used compared to the number of observations. 
#However, AIC can only be compared between models on the same data set, but it provides a means for model 
#selection. The preferred model is the one with the minimum AIC. 

PredictTrain = predict(qualityLog, type = "response") #the type = "response" tells the predict function 
#to give probabilities
summary(PredictTrain)


tapply(PredictTrain, qualityTrain$PoorCare, mean)
#So for the true poor care cases the average probability of poor care is .44. For all the true
#good care cases we predict an average probability of .19.

#Model 2
qualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(qualityLog2)

#Compute confusion/classification matrices based on different threshold values

#Threshold value of t = .5
table(qualityTrain$PoorCare, PredictTrain > .5)
#For 70 cases we predict good care correctly
#For 10 cases we predict poor care correctly
#For 4 cases we predict poor care, but they actually received good care
#For 15 cases we predict good care, but they actually received poor care

#Sensitivity = 10/25 = .4
#Specificity = 70/74 = .95

#Threshold value of t = .7
table(qualityTrain$PoorCare, PredictTrain > .7)
#Sensitivity = .32
#Specificity = .99

table(qualityTrain$PoorCare, PredictTrain > .2)
#Sensitivity = .64
#Specificity = .73

#Generate Receiver Operator Characteristic (ROC) Curves
library(ROCR)
ROCRpred = prediction(PredictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
#Performance function defines what we'd like to plot on the x and y axes on our ROC Curve
#"tpr" is True Positive Rate, "fpr" is False Positive Rate

plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0, 1, .1), text.adj = c(-.2, 1.7))



####
predictTest = predict(qualityLog, type = "response", newdata = qualityTest)
#Compute test set area under the curve (AUC) with:
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

#MIT Analytics Edge Week 3 - Election Forecasting

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
polling <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/PollingData.csv')
str(polling)
table(polling$Year)
summary(polling)

###Approaches to dealing with missing data

##Delete the missing observations. Here that would mean throwing away over 50% of the observations. 
##We want to predict for all states, not just for the ones that reported all their variable values.

##Remove variables with missing values. Here that would mean getting rid of the Rasmussen and 
##Survey USA, but we want to keep these since these will be qualitatively different from aggregate variables,
##such as DiffCount and PropR.

##Fill missing data points with average values

##Multiple Imputation
##Here we fill in the missing values based on the non-missing values.
##For instance, if the Rasmussen variable is very negative, then a missing SurveyUSA value is likely to be negative
##Just like sample.split, results will differ between runs unless you fix the random seed.
##This is mathematically sophisticated, but we can use it easily through pre-existing R libraries.
##Will use the Multiple Imputation by Chained Equations (mice) package. 
library(mice)

#For our multiple imputation to be useful, we have to be able to find out the values of our missing variables 
#without using the outcome of Republican. So we limit our data frame to just the four polling related variables.
simple <- polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
set.seed(144)
imputed <- complete(mice(simple))
#Output shows us that 5 rounds of imputation have been run, and now all of the variables have been filled in.
summary(imputed)

#Last step in imputation process is to copy the Rasmussen and SurveyUSA variables back into our original
#data frame, which has all the variables for the problem. 
polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA
summary(polling)

#Split data into training and testing sets. We will train on data from 2004 and 2008 elections. 
train <- subset(polling, Year == 2004 | Year == 2008)
test <- subset(polling, Year == 2012)

#Baseline Model
#Look at the breakdown of the dependent variable in the train set
table(train$Republican)
#In 47 of the 100 training observations, Democrat won the state whereas in 53 of the 100 training observations
#the Republican won the state. 
#So our simple baseline model is always going to predict the more common outcome, which is that the 
#Republican is always going to win the state. So the simple baseline model will have accuracy of 53% on 
#the training set. --Not a very credible model

#Try a smarter baseline model. Take Rasmussen and make a prediction based on who the poll said was winning the 
#state. 
#To compute this smart baseline we will use the sign() function. If it is passed a positive number it retrns
#+1 and if it is passed a negative number it returns -1, and if it is passed 0 it returns 0.

#If the Rasmussen variable is positive, it signifies that the Republican is polling ahead, so sign return 1. 
#If the Democrat is winning in the Rasmussen poll, it will be negative so the sign() returns -1.
#If Rasmussen poll had a tie it would be 0 and so sign returns 0.
table(sign(train$Rasmussen))
#We can see that in 55 of the 100 training set observations, the smart baseline predicted that the Republican
#was going to win. In 42 of the 100 training set observations, it predicted that the Democrat was going to win.
#In 3 instances it was inconclusive.
#Let's compare how well this smart baseline model performed with who actually won the state.  
table(train$Republican, sign(train$Rasmussen))
#Based on the output, when the Democrat actually won, our smart baseline model predicted 42/47 correctly.
#When the Republican actually won, our smart baseline model predicted 52/53 correctly. 
#This model does much better than the naive baseline, which was simply predicting the Republican would
#always win and would make 47 mistakes on the same data. This is much more reasonable baseline model that we 
#can compare with our logistic regression model.

#Before building the models, consider multicollinearity.
cor(train[ , 2:7])

#Let's first consider the case of building a logistic regression model with just one variable. Let's use
#the variable that's most highly correlated with Republican.

mod1 = glm(Republican ~ PropR, data = train, family = binomial)
summary(mod1)
#PropR (proportion of polls that said Republican won) is very significant, high coefficient and AIC = 19.8. 
#So this seems like a reasonable model.
#Let's see how it does on predicting the Republican outcome on the training set. 
pred1 = predict(mod1, type = "response")
table(train$Republican, pred1 >= .5)
#Here, when a Democrat actually won the state, our model predicted correctly 45/47 = .957 times.
#When a Republican actually won the state, our model predicted correctly 51/53 = .962 times.
#So we see that on the training set, this model with one variable as a prediction makes 4 mistakes, which 
#is just about the same as our smart baseline model. 

#Can we improve on this performance by adding another variable?
#Since there's so much multicollinearity, we might be searching for a pair of variables that has a relatively
#lower correlation with each other because they might kind of work together to improve  the prediction overall. 
#If 2 variables are highly correlated, they're less likely to improve predictions together since they're so
#similar in their correlation structure.

#Least correlated pairs of variables are the Rasmussen and DiffCount or the SurveyUSA and DiffCount.
#Let's try one of these pairs.
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = train, family = binomial)
summary(mod2)
pred2 = predict(mod2, type = "response")
table(train$Republican, pred2 >= .5)
#Here we made 1 fewer mistake on the training set.So it's a little better than the smart baseline, 
#but not too impressive.
#Also note AIC = 18.44 which is smaller which suggests a stronger model, but neither of the variables 
#are that significant.

#Let's evaluate our models on the testing set. 
table(test$Republican, sign(test$Rasmussen))
#We can see that the smart baseline predicted 18 of the 24 Democratic wins correctly and 
#all 21 of the Republican wins correctly. So 4 mistakes and 2 inconclusive results with this model on the 
#testing set. 

TestPrediction = predict(mod2, newdata = test, type = "response")
table(test$Republican, TestPrediction >= .5)
#In all but one of the 45 cases, our model is correct.

#We could have tried changing the threshold from .5 to other values and computed out an ROC curve, 
#but this doesn't make much sense in this setting where we're just trying to accurately predict the outcome 
#of each state and we don't care more about one sort of error--when we predicted Republican and it was 
#Democrat--than the other, where we predicted Democrat and it was actually Republican.

#Let's understand the one mistake.
subset(test, TestPrediction >= .5 & Republican == 0)
#Looking at the predictor variables, we see why we made the mistake. The Rasmussen poll gave the Republican
#a 2 percentage point lead, SurveyUSA called a tie, DiffCount said there were 6 more polls that predicted
#Republican than Democrat, and 2/3 of the polls predicted the Republican was going to win. But in this case, 
#the Republican didn't win. Barack Obama won Florida in 2012 over Mitt Romney.

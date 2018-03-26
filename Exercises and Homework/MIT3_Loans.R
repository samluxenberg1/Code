#MIT Analytics Edge Week 3 - Loans

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
loans <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/loans.csv')
str(loans)
table(loans$not.fully.paid)
summary(loans)

#Analyze dataset with missing values
missing <- subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
str(missing)
table(missing$not.fully.paid)
#To predict risk for loans with missing values we need to fill in the missing values instead of removing them.
library(mice)
set.seed(144)
vars.for.imputation <- setdiff(names(loans), "not.fully.paid")
imputed <- complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] <- imputed
#To do this imputation we set vars.for.imputation to all variables in the data frame except for not.fully.paid, 
#to impute the values using all of the other independent variables.
#Imputation predicts the missing variable values for a given observation using the variable values that are 
#reported. We called the imputation on a data frame with the dependent variable not.fully.paid removed, 
#so we predicted the missing values using only other indepdent variables. 

#Split the data
set.seed(144)
split <- sample.split(loans$not.fully.paid, SplitRatio = .7)
train <- subset(loans, split == TRUE)
test <- subset(loans, split == FALSE)

#Build Model
Model1 = glm(not.fully.paid ~ ., data = train, family = binomial)
summary(Model1)

#Applicants A and B have FICO scores 700 ad 710 respectively. They have the same characteristics for all other
#variables.Let Logit(A) = log odds of loan A not being paid back in full according to our model, and 
#define Logit(B) similarly for B. What is Logit(A) - Logit(B)?
700*(-.009406) - (710*(-.009406))
#Let O(A) = odds of loan A not being paid back in full, and define O(B) similarly. What is O(A)/O(B)?
#O(A)/O(B) = e^ln(O(A)/O(B)) = e^(ln(O(A)) - ln(O(B))) = e^(Logit(A) - Logit(B)) = e^(.09406) = 1.0986

#Predict the probability of the test set not being paid back in full
predicted.risk = predict(Model1, newdata = test, type = "response")
table(test$not.fully.paid, predicted.risk >= .5)
#Accuracy
(2400+3)/(2400+13+457+3)
#Baseline Model
#Model always predicts loans are paid back in full
#Baseline Accuracy
(2400+13)/(2400+13+457+3)

#Compute test set AUC
library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

#Our model will classify whether or not an applicant will fully pay back his/her loan correctly 
#about 67% of the time.

#Smart baseline
#LendingClub.com assigns the interest rate based on their estimate of that loan's risk. This variable, int.rate, 
#is independent in our dataset. We will use the loan's interest rate as the smart baseline model to order the 
#loans according to risk. 

#Use training set to build bivariate logistic regression model
bivariate = glm(not.fully.paid ~ int.rate, data = train, family = binomial)
summary(bivariate)
cor(train$fico, train$int.rate)

predicted.risk_b = predict(bivariate, newdata = test, type = "response")
summary(predicted.risk_b)
table(test$not.fully.paid, predicted.risk_b >= .5)
#Since the maximum predicted probability is .4266, if there is a threshold of .5, then no one would be 
#predicted to not fully  pay back the loan.

#Test set AUC
ROCRpred.bivariate = prediction(predicted.risk_b, test$not.fully.paid)
as.numeric(performance(ROCRpred.bivariate, "auc")@y.values)
#The smart baseline classifies correctly about 62.4% of the time.

#Computing the profitability of an investment
#If a loan is paid back in full, then the investor makes interest on the loan.However, if the loan is not
#paid back, the investor loses the money invested. Therefore, the investor should seek loans that best balance
#this risk and reward. 

#To compute interest revenue, consider a $c investment in a loan that has an annual interest rate r over a
#period of t years. Using continuous compounding of interest, this investment pays back c*exp(rt) dollars
#by the end of t years.

#How much does a $10 investment with an annual interest rate of 6% pay back after 3 years?
10*exp(.06*3)

#Consider the case where an investor  made a $c investment, but it was not paid back in full. 
#Assume, conservatively, that no money was received from the borrower. 
#The investor's profit is then -$c.

#Investment Strategy
#Assume $1 investment and compute the profit for each loan in the test set. Also each loan is a 3-year loan.  
test$profit <- exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] <- -1
#What is the maximum profit of a $10 investment in any loan in the testing set?
max(test$profit)*10

#Investment Strategy Based on Risk
#Analyze an investment strategy in which the investor only purchases loans with a high interest rate (>= 15%)
#but amongst these loans, selects the ones with the lowest predicted risk of not being paid back in full. 
#We will model an investor who invests $1 in the 100 most promising loans. 
test$predicted.risk <- predicted.risk
highInterest <- subset(test, int.rate >= .15)
#Average profit of a $1 investment
summary(highInterest$profit)
table(highInterest$not.fully.paid)

#Determine the 100th smallest predicted probability of not paying back in full by sorting the predicted risks
#in increasing order and selecting the 100th element in this list. Find the highest predicted risk that we will
#include.

cutoff <- sort(highInterest$predicted.risk, decreasing = FALSE)[100]
selectedLoans <- subset(highInterest, predicted.risk <= cutoff)

#What is the profit of the investor, who invested $1 in each of these 100 loans?
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)

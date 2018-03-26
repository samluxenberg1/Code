#MIT Analytics Edge Week 2 - NBA Stats

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
NBA_test <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/NBA_test.csv')
NBA <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/NBA_train.csv')
  str(NBA_test)
table(NBA$W, NBA$Playoffs)
NBA$PTSdiff <- NBA$PTS - NBA$oppPTS
ggplot(aes(x = PTSdiff, y = W), data = NBA) +
  geom_point()

WinsReg = lm(W ~ PTSdiff, data = NBA)
summary(WinsReg)

PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg)

#Compute residuals
PointsReg$residuals

#Sum of Squared Error
SSE = sum(PointsReg$residuals^2)
#Root Mean Squared Error(RMSE) ~ avg error we make in our predictions
RMSE = sqrt(SSE/nrow(NBA)) 
#RMSE = 184.41, so on average we make an error of about 184.4 points
#Not so bad since average points in a season is 8370
mean(NBA$PTS)

#Let's improve our model by getting rid of the least statistically significant variables (one at a time)
#Since Turnovers have the least statistical significance (highest p-value), let's remove that first.
PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)
summary(PointsReg2)
#Since R-sq only decrease very slightly when removing Turnovers, we seem to be justified in removing it 
#from our model
PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
summary(PointsReg3)
#Same as for Turnovers. We are justified in removing Defensive Rebounds from the model.
PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsReg4)
#Same
#So our model is much simpler, all variables are siginificant
#Let's look at SSE and RMSE to make sure we didn't inflate those too much when we removed variables.

SSE_4 = sum(PointsReg4$residuals^2) #28,421,465
RMSE_4 = sqrt(SSE_4/nrow(NBA)) #184.4049

#Just about the same amount of error with a simpler and more interpretable model

#Let's make predictions for the 2012-2013 NBA season
PointsPredictions = predict(PointsReg4, newdata = NBA_test)

#How good is our prediction?
#Compute out-of-sample R-sq. This is a measurement of how well the model predicts on the test data.
#The R-sq value we had before from our model was .8991 is the measure of an in-sample R-sq 
#i.e. how well the model fits the training data.
#Out-of-sample R-sq gives measure of our prediction's goodness of fit
SSE = sum((PointsPredictions - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSE/SST #R-sq
RMSE = sqrt(SSE/nrow(NBA_test))
#Our model has average error of about 196 points.
mean(NBA$PTS)

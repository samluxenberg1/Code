#MIT Analytics Edge Week 2 -  Climate Change

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
climate_change <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/climate_change.csv')
str(climate_change)

#Create training data: all data up to and including 2006
climate_train <- subset(climate_change, Year <= 2006)
climate_test <- subset(climate_change, Year > 2006)

#Build linear regression model to predict Temp, using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, AEROSOLS
#as independent variables.
climatelm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_train)
summary(climatelm)
cor(climate_train)

#Build simpler model
climatelm2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data = climate_train)
summary(climatelm2)

#R provides a function, step, that will automate the procedure of trying different combinations of variables to
#find a good compromise of model simplicity and R-sq. This tradeoff is formalized by the 
#Akaike information criterion (AIC) - it can be informally thought of as the quality of the model with a penalty
#for the number of variables in the model.
climatelm_step = step(climatelm)
summary(climatelm_step)

#Predictions for test data
TempPredictions = predict(climatelm_step, newdata = climate_test)
SSE = sum((TempPredictions - climate_test$Temp)^2)
SST = sum((mean(climate_train$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST #R-sq
RMSE = sqrt(SSE/nrow(climate_test))
mean(climate_train$Temp)

TempPredictions2 = predict(climatelm2, newdata = climate_test)
SSE = sum((TempPredictions2 - climate_test$Temp)^2)
SST = sum((mean(climate_train$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST #R-sq
RMSE = sqrt(SSE/nrow(climate_test))
mean(climate_train$Temp)

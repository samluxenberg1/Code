#Linear Regression with Baseball

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
baseball <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/baseball.csv')
str(baseball)

#Verify claims from Moneyball
moneyball <- subset(baseball, Year < 2002)
moneyball$RD <- moneyball$RS - moneyball$RA

ggplot(aes(x = RD, y = W), data = moneyball) +
  geom_point()

#Build linear regression model predicting the number of wins in the regular season 
WinsReg = lm(W ~ RD, data = moneyball)
summary(WinsReg)

#Wins = 80.8814 + .1058*RunsDifference
#Money ball predicted that in order to at least 95 wins a team would have to score at least 135 more runs
#WTS: W >= 95, so 80.8814 + .1058*RunsDifference >= 95 => RD >= (95-80.8814)/.1058 = 133.4

#Now need to predcit how many runs a team will score and how many runs a team will allow
RunsReg = lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg)
#OBP = 2917.42, SLG = 1637.93, BA = -368.97
#So all else being equal, a team with a lower batting average will score more runs
#Multi-collinearity -- These 3 hitting stats are highly correlated so it's hard to interpret the coefficients
#of our model

RunsReg = lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsReg)
#R-sq still about .93 and both OBP and SLG still positive. So this model is simpler with only 
#2 independent variable. So it's overall a better model.
#Since OBP and SLG are on about the same scale and the coefficient for OBP is a lot higher than SLG, 
#we have confirmed that OBP is the most important batting statistic for predicting runs scored

#Predict how many runs a team will allow
RunsReg = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(RunsReg)


#Playoffs
teamRank <- c(1, 2, 3, 3, 4, 4, 4, 4, 5, 5)
#How well do these rankings correlate with regular season wins?
wins2012 <- c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013 <- c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)
cor.test(teamRank, wins2012)
cor.test(teamRank, wins2013)
cor.test(wins2012, wins2013)

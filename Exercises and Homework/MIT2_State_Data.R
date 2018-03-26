#MIT Analytics Edge Week 2 - State Data
setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
library(ggplot2)
data(state)
statedata <- cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)
ggplot(aes(x = x, y = y), data = statedata) +
  geom_point()

#Which region of the US has the highest average high school graduation rate of all states in the region?
tapply(statedata$HS.Grad, statedata$state.region, mean)

#Box plot of murder rate by region
ggplot(aes(x = state.region, y = Murder), data = statedata) +
  geom_boxplot()

#Which state is the outlier in the Northeast?
which.max(subset(statedata, state.region == "Northeast")$Murder) #= 6
subset(statedata, state.region == "Northeast")[6, ]

#Build model to predict life expectancy by state
LifeExpLM = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(LifeExpLM)

#What is the interpretation for the coefficient of Income?
#-.0000218 means for 1 unit increase in Income, there is a .0000218 unit decrease in Life Expectancy.

ggplot(aes(x = Income, y = Life.Exp), data = statedata) +
  geom_point()

#Although income is an insignificant variable in the model, this does not mean there is no association between
#income and life expectancy (after all in the plot of Life Exp vs Income there is a somewhat positive
#linear correlation even though the coefficient of income in our model is negative). However, in the 
#presence of other variables, Income does not add statistically significant explanatory power to the model. 
#This means that multicollinearity is probably an issue.

#Refining the model
#Removing variables one at a time is called "backward variable selection." This is important due to 
#the multicollinearity issue-removing one insignificant variable may make another previously insignificant
#variable become significant.

#Remove Area
LifeExpLm2 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpLm2)
#Remove Illiteracy
LifeExpLm3 = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpLm3)
#Remove Income
LifeExpLm4 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpLm4)

#When we remove insignificant variables, the "Multiple R-Sq" will always be worse, but only slightly worse. 
#This is due to the nature of the linear regression model. It is always possible for the regression model
#to make a coefficient zero, which would be the same as removing the variable from the model. The fact
#that the coefficient in the initial model is not zero means it must be helping the R-Sq value, even if
#it is only a very small improvement. So when we force the variable to be removed, it will decrease the R-Sq a
#little bit. However, this small decrease is worth it to have a simpler model. 

#On the contrary, when we remove insignificant variables, the "Adjusted R-Sq" will frequently be better. 
#This value accounts for the complexity of the model, and thus tends to increase as insignificant
#variables are removed, and decrease as insignificant variables are added.

#Which state do we predict to have the lowest Life Expectancy? (only using training data for this one)
PredLifeExp = predict(LifeExpLm4)
sort(PredLifeExp)
#Which state actually has the lowest Life Expectancy?
which.min(statedata$Life.Exp)
statedata[40, ]

#Highest?
which.max(statedata$Life.Exp)
statedata[11, ]

#Look at vector of residuals (the difference between predicted and actual values)
resvec <- c(abs(PredLifeExp - statedata$Life.Exp))
#Which state has the smallest absolute error?
sort(resvec)

#Alternatively
sort(abs(PredLifeExp$residuals))

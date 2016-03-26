#MIT Analytics Edge Week 4 - Boston Housing Data

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
boston <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/boston.csv')
str(boston)
#Interested in building a model of how prices vary by location across a region
library(ggplot2)
MIT <- subset(boston, TRACT == 3531)
ggplot(aes(x = boston$LON, y = boston$LAT), data = boston) +
  geom_point(aes(color = factor(CHAS))) +
  geom_point(data = MIT, aes(color = "black"))#FIGURE OUT LATER
  

#Or
plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS == 1], boston$LAT[boston$CHAS == 1], col = "blue", pch = 19)
points(boston$LON[boston$TRACT == 3531], boston$LAT[boston$TRACT == 3531], col = "red", pch = 19) 
summary(boston$NOX)
points(boston$LON[boston$NOX >= .55], boston$LAT[boston$NOX >= .55], col = "green", pch = 19)

plot(boston$LON, boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)
#See all census tracts with above average housing prices  

plot(boston$LAT, boston$MEDV)
plot(boston$LON, boston$MEDV)

latlonlm = lm(MEDV ~ LAT + LON, data = boston)
summary(latlonlm)
#R-Sq is low
#Latitude is not significant, which means the north-south differences aren't really going to be used at all
#Longitude is significant and it is negative. So as we go towards the ocean, housing prices decrease linearly
#This seems rather unlikely
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)
#In red are the above median values of housing prices for the census tracts
#What does the linear regression model think is above median
latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values >= 21.2], boston$LAT[latlonlm$fitted.values >= 21.2], col = "blue", pch = "$")
#Linear regression model not doing such a great job and is completely ignoring everything to the right side of the picture


##Regression Trees
library(rpart)
library(rpart.plot)
latlontree <- rpart(MEDV ~ LAT + LON, data = boston)
prp(latlontree)
#In regression trees we predict a number
#That number is the average of the median house prices in that bucket or leaf
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues >= 21.2], boston$LAT[fittedvalues >= 21.2], col = "blue", pch = "$")
#We're still making mistakes, but we're able to make a nonlinear prediction on latitude and longitude
#Interesting but the tree was complicated
#Can we get most of this effect with a much simpler tree?
#Change the minbucket size
latlontree <- rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
plot(latlontree)
text(latlontree)
#There are far fewer splits and more interpretable 
#the left-hand side of the tree corresponds to right hand side
#of the map and the right side of the tree corresponds to the left hand side of the map
#while right hand branch on right hand side of the picture
plot(boston$LON, boston$LAT)
abline(v = -71.07)
abline(h = 42.21)
abline(h = 42.17)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)

library(caTools)
set.seed(123)
split <- sample.split(boston$MEDV, SplitRatio = .7)
train <- subset(boston, split == TRUE)
test <- subset(boston, split == FALSE)
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
summary(linreg)
linreg.pred = predict(linreg, newdata = test)
linreg.sse = sum((linreg.pred - test$MEDV)^2)
# = 3037.088
#Can we beat this using trees?

tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
prp(tree)
#Variable at the top is the most important split
#Rooms are the most important split. It also appears three times (nonlinear on the number of rooms)
#Pollution appears in there twice, it's in some sense nonlinear on the amount of pollution if it's great than a 
#certain amount and if it's less than a certain amount it does different things
#crime and age are in there
#Things that were important for the linear regression that don't appear in the tree are:
#Pupil to teacher ratio, DIS
#The two processes are doing different things, but how do they compare?
tree.pred <- predict(tree, newdata = test)
tree.sse = sum((tree.pred - test$MEDV)^2)

#Since the SSE for the regression tree was higher than for linear regression, regression trees are not as good for
#this problem. 
#What this says, given what we saw with the latitude and longitude is that latitude and longitude are nowhere near
#as useful for predicting as these other variables are


##Cross-Validation
library(caret)
library(e1071)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = (0:10)*.001)
0:10*.001 #these are the values of cp that caret will try
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS +CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, 
           method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr
#it decided that cp = .001 is the best because it had the best root mean square error (RMSE)

#to see the tree that that cp value corresponds to: 
best.tree = tr$finalModel
prp(best.tree)

#Will this beat the linear regression model?
best.tree.pred = predict(best.tree, newdata = test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)
# = 3675.766 This tree is better on the test set than the original tree, but the linear regression model still did 
#better. While it's not as good, Cross-Validation did improve the tree performance
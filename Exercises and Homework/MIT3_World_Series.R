#MIT Anlaytics Edge Week 3 - Predicting Baseball World Series Champion

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
baseball <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/baseball.csv')
nrow(baseball)
table(baseball$Year)
#Total number of years in the dataset ('72, '81, '94, '95 are missing)
length(table(baseball$Year))
str(baseball)
baseball <- subset(baseball, Playoffs == 1)
nrow(baseball)
table(baseball$Year)
table(table(baseball$Year))

PlayoffTable <- table(baseball$Year)
PlayoffTable[c("1990", "2001")]
#Because PlayoffTable is an object, not a function, we look up elements in it with square brackets instead of 
#parentheses. We build the vector of years to be passed with the c() function. Because the names of PlayoffTable
#are strings and not numbers, we need to pass "1990" and "2001". 

#We want to look up the number of teams in the playoffs for each team/year pair in the data set, and store it as
#NumCompetitors.
baseball$NumCompetitors <- PlayoffTable[as.character(baseball$Year)]

#How many playoff team/year pairs are there in our dataset from years where 8 teams were invited to the playoffs?
table(baseball$NumCompertitors)

#Bivariate Models for predicting World Series Winner (RankPlayoffs = 1)
baseball$WorldSeries <- as.numeric(baseball$RankPlayoffs == 1)

#How many observations in our dataset do we have where the team didn not win the World Series?
table(baseball$WorldSeries)

#When we're not sure which of our variables are useful for predicting a particular outcome, 
#it's often helpful to build bivariate models. 

Model1 = glm(WorldSeries ~ Year, data = baseball, family = binomial)
summary(Model1)#AIC = 232.35
Model2 = glm(WorldSeries ~ RS, data = baseball, family = binomial)
summary(Model2)
Model3 = glm(WorldSeries ~ RA, data = baseball, family = binomial)
summary(Model3)#AIC = 237.88
Model4 = glm(WorldSeries ~ W, data = baseball, family = binomial)
summary(Model4)
Model5 = glm(WorldSeries ~ OBP, data = baseball, family = binomial)
summary(Model5)
Model6 = glm(WorldSeries ~ SLG, data = baseball, family = binomial)
summary(Model6)
Model7 = glm(WorldSeries ~ BA, data = baseball, family = binomial)
summary(Model7)
Model8 = glm(WorldSeries ~ RankSeason, data = baseball, family = binomial)
summary(Model8)#AIC = 238.75
Model9 = glm(WorldSeries ~ OOBP, data = baseball, family = binomial)
summary(Model9)
Model10 = glm(WorldSeries ~ OSLG, data = baseball, family = binomial)
summary(Model10)
Model11 = glm(WorldSeries ~ NumCompertitors, data = baseball, family = binomial)
summary(Model11)#AIC = 230.96
Model12 = glm(WorldSeries ~ League, data = baseball, family = binomial)
summary(Model12)

#Multivariate Models for predicting the World Series Winner
MultiMod = glm(WorldSeries ~ Year + RA + RankSeason + NumCompertitors, data = baseball, family = binomial)
summary(MultiMod)#AIC = 236.37

#Often variables that were significant in bivariate models are no longer significant in multivariate
#analysis due to correlation between the variables. 
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

MultiMod2 = glm(WorldSeries ~ Year + RA, data = baseball, family = binomial)
summary(MultiMod2)#AIC = 233.88
MultiMod3 = glm(WorldSeries ~ Year + RankSeason, data = baseball, family = binomial)
summary(MultiMod3)#AIC = 233.55
MultiMod4 = glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = binomial)
summary(MultiMod4)#AIC = 232.9
MultiMod5 = glm(WorldSeries ~ RankSeason + RA, data = baseball, family = binomial)
summary(MultiMod5)#AIC = 238.22
MultiMod6 = glm(WorldSeries ~ NumCompetitors + RA, data = baseball, family = binomial)
summary(MultiMod6)#AIC = 232.74
MultiMod7 = glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = binomial)
summary(MultiMod7)#AIC = 232.52

#This seems to confirm the claim by Billy Beane in Moneyball that all that matters in the playoffs is luck, 
#since the number of competitors has nothing to do with the quality of the teams.

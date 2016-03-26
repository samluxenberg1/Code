#Captstone Project
#Global Terrorism
#Source: National Consortium for the Study of Terrorism and Responses to Terrorism (START). (2014). 
#Global Terrorism Database [gtd_06to13_0814dist]. Retrieved from http://start.umd.edu/gtd
#1/1/2011 - 12/31/2014

#Research Question
#Given various characteristics of terrorist attacks, is it possible to accurately predict the perpetrators of 
#an attack? If so, this model could help identify behaviors of specific terrorist groups and what typical 
#consequences are of of their attacks. 

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
gtd <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/gtd_11to14_0615dist.csv')

#Variables without missing values
#eventid
#iyear
#imonth
#iday
#extended
#country
#country_txt
#region
#region_txt
#vicinity
#crit1
#crit2
#crit3
#doubtterr
#multiple
#success
#suicide
#attacktype1
#attacktype1_txt
#targtype1
#targtype1_txt
#gname
#guncertain1
#claimed
#weaptype1
#weaptype1_txt
#property
#ishostkid

#Variables added
gtd$mult_attack_types <- ifelse(is.na(gtd$attacktype2), 0, 1)
gtd$mult_targtypes <- ifelse(is.na(gtd$targtype2), 0, 1)
gtd$mult_weap_types <- ifelse(is.na(gtd$weaptype2), 0, 1)
gtd$mult_natlty <- ifelse(is.na(gtd$natlty2), 0, 1)

#Variables to fix missing values
gtd$nwoundus <- ifelse(is.na(gtd$nwoundus), 0, gtd$nwoundus)
gtd$nkillus <- ifelse(is.na(gtd$nkillus), 0, gtd$nkillus)
gtd$nperps <- ifelse(is.na(gtd$nperps), 1, gtd$nperps)
gtd$nperpcap <- ifelse(is.na(gtd$nperpcap), 0, gtd$nperpcap)

gtd_clean <- data.frame(gtd$eventid, gtd$iyear, gtd$imonth, gtd$iday, gtd$extended, 
                        gtd$country, gtd$country_txt, gtd$region, gtd$region_txt, gtd$vicinity,
                        gtd$crit1, gtd$crit2, gtd$crit3, gtd$doubtterr,
                        gtd$multiple, gtd$success, gtd$suicide, gtd$attacktype1, 
                        gtd$attacktype1_txt, gtd$targtype1, gtd$targtype1_txt, gtd$gname, gtd$guncertain1,
                        gtd$claimed, gtd$weaptype1, gtd$weaptype1_txt, gtd$property,
                        gtd$ishostkid, gtd$mult_natlty, gtd$mult_targtypes, 
                        gtd$mult_weap_types, gtd$mult_attack_types, gtd$nwoundus, gtd$nkillus, 
                        gtd$nperps, gtd$nperpcap)

gtd_clean$gtd.extended <- as.factor(gtd_clean$gtd.extended)
gtd_clean$gtd.vicinity <- as.factor(gtd_clean$gtd.vicinity)
gtd_clean$gtd.crit1 <- as.factor(gtd_clean$gtd.crit1)
gtd_clean$gtd.crit2 <- as.factor(gtd_clean$gtd.crit2)
gtd_clean$gtd.crit3 <- as.factor(gtd_clean$gtd.crit3)
gtd_clean$gtd.doubtterr <- as.factor(gtd_clean$gtd.doubtterr)
gtd_clean$gtd.multiple <- as.factor(gtd_clean$gtd.multiple)
gtd_clean$gtd.success <- as.factor(gtd_clean$gtd.success)
gtd_clean$gtd.suicide <- as.factor(gtd_clean$gtd.suicide)
gtd_clean$gtd.guncertain1 <- as.factor(gtd_clean$gtd.guncertain1)
gtd_clean$gtd.claimed <- as.factor(gtd_clean$gtd.claimed)
gtd_clean$gtd.property <- as.factor(gtd_clean$gtd.property)
gtd_clean$gtd.ishostkid <- as.factor(gtd_clean$gtd.ishostkid)
gtd_clean$gtd.mult_natlty <- as.factor(gtd_clean$gtd.mult_natlty)
gtd_clean$gtd.mult_targtypes <- as.factor(gtd_clean$gtd.mult_targtypes)
gtd_clean$gtd.mult_weap_types <- as.factor(gtd_clean$gtd.mult_weap_types)
gtd_clean$gtd.mult_attack_types <- as.factor(gtd_clean$gtd.mult_attack_types)

#Want to be able to predict which terrorist groups carry out which attacks through classification  with CART or random forests. 
#However, there are 562 different terrorist organizations in this dataset. To simplify this problem, let's instead see if we can use CART 
#and random forest models to correctly classify the top 5 most common terrorist organizations. 

head(sort(table(gtd_clean$gtd.gname), decreasing = TRUE), n = 6)
#The top 5 (not counting unknown) are Taliban, Islamic State of Iraq and the Levant (ISIL) (aka ISIS), 
#Al-Shabaab, Boko Haram, Al-Qa`ida in the Arabian Peninsula (AQAP).

#Subset the data to only include these 5 organizations
top5 <- c("Taliban", "Islamic State of Iraq and the Levant (ISIL)", "Al-Shabaab", "Boko Haram", "Al-Qa`ida in the Arabian Peninsula (AQAP)")
gtd_top5 <- gtd_clean[gtd_clean$gtd.gname %in% top5, ]
gtd_top5$gtd.gname <- droplevels(gtd_top5$gtd.gname) 

#Split the data
library(caTools)
set.seed(123)
split <- sample.split(gtd_top5$gtd.gname, SplitRatio = .7)
top5Train <- subset(gtd_top5, split == TRUE)
top5Test <- subset(gtd_top5, split == FALSE)

#CART Models
library(rpart)
library(rpart.plot)

#Baseline Model (always predicts the most common outcome)
#Accuracy about 35.28%
table(top5Train$gtd.gname)
1971/(489+1104+892+1130+1971)

CART1 <- rpart(gtd.gname ~ gtd.vicinity + gtd.success + gtd.suicide + gtd.property, data = top5Train, method = "class")
prp(CART1)
#This model says that if it is unknown whether or not there is property damage and if it isn't a suicide attack
#then this model predicts the attack to belong to Al-Shabaab. If it was a suicide attack then the model predicts
#this as an ISIS attack. On the other hand, if we do know for sure whether there is property damage (one way or the
#other), then this model predicts the Taliban is responsible.
#Note that whether or not there was property damage and whether or not it was a suicide attack are the most
#important variables in this model.

#Accuracy of CART1 about 37.64% (slightly better than baseline)
predictCART1 = predict(CART1, newdata = top5Test, type = "class")
table(top5Test$gtd.gname, predictCART1)
(0+140+16+745)/nrow(top5Test)

CART1$variable.importance

CART2 <- rpart(gtd.gname ~ gtd.iyear + gtd.imonth + gtd.iday + gtd.extended + gtd.region_txt, data = top5Train, method = "class")
prp(CART2)
#According to this model, the region in which the incident occured was the most important variable followed by
#year and whether or not the duration of the incident extended to more than 24 hours.

#Accuracy of CART2 about 80.8%
predictCART2 = predict(CART2, newdata = top5Test, type = "class")
table(top5Test$gtd.gname, predictCART2)
(82+363+161+484+844)/nrow(top5Test)
#This significant accuracy improvement is probably due to the fact that some terrorist groups only operate in certain regions,
#so perhaps this was obvious and not too helpful.

CART2$variable.importance

CART3 <- rpart(gtd.gname ~ gtd.crit1 + gtd.crit2 + gtd.crit3 + gtd.multiple + gtd.doubtterr + gtd.attacktype1_txt, data = top5Train, method = "class")
prp(CART3)
#Interpretation: If the attack is part of a series of connected but separate attackes and the attack type is Armed Assault, Assassination,
#Facility/Infrastructure Attack, or Hostage Taking then the model predicts Boko Haram, but if the attack type is Bombing/Explosion, 
#Hijacking, unarmed assault or uknown then it will be ISIS. 
#On the other hand, if the attack is a single unconnected incident and there is a possibility there it may not be a terror attack, 
#then it can go for Al-Shaba or Taliban based on the attack type. If it's a single incident with no doubt that it was indeed a terror
#attack, then it's the Taliban.
#Note most important variables: whether or not the incident was connected to a series of incidents and then attack type.

#Accuracy of CART3 about 45.15% (significant improvement over CART1 and Baseline)
predictCART3 = predict(CART3, newdata = top5Test, type = "class")
table(top5Test$gtd.gname, predictCART3)
(0+95+96+218+672)/nrow(top5Test)

CART3$variable.importance

CART4 <- rpart(gtd.gname ~ gtd.targtype1_txt + gtd.guncertain1 + gtd.claimed + gtd.weaptype1_txt + gtd.ishostkid, data = top5Train, method = "class")
prp(CART4)
title(main = "CART Model 4 Decision Tree")
#Most important variables are the target type and whether or not the organization claimed responsibility as well as weapon type.

#Accuracy of CART 4 about 49.29%
predictCART4 = predict(CART4, newdata = top5Test, type = "class")
table(top5Test$gtd.gname, predictCART4)
(193+161+264+562)/nrow(top5Test)

CART4$variable.importance

CART5 <- rpart(gtd.gname ~ gtd.mult_natlty + gtd.mult_targtypes + gtd.mult_weap_types + gtd.mult_attack_types, data = top5Train, method = "class")
prp(CART5)
#Most important variable is whether or not the terrorists had multiple types of weapons

#Accuracy of CART5 about 37.93%
predictCART5 = predict(CART5, newdata = top5Test, type = "class")
table(top5Test$gtd.gname, predictCART5)
(0+0+131+0+777)/nrow(top5Test)

CART5$variable.importance

CART6 <- rpart(gtd.gname ~ gtd.nwoundus + gtd.nkillus + gtd.nperps + gtd.nperpcap, data = top5Train, method = "class")
prp(CART6)
#This model predicts Boko Haram if there were at least 8 terrorists involved in the incident and the Taliban if there were 7 or fewer.
#Most important variable was the number of terrorists involved in the incident.
#Despite many threats to America, the number of us fatalities and us wounded were not terribly significant.

#Accuracy of CART6 about 36.09%
predictCART6 = predict(CART6, newdata = top5Test, type = "class")
table(top5Test$gtd.gname, predictCART6)
(0+0+44+0+820)/nrow(top5Test)

CART6$variable.importance

#Now that we've used all the variables we want to, let's combine the most important ones together in 1 model. 
CART7_region <- rpart(gtd.gname ~ gtd.region_txt + gtd.nperps + gtd.mult_weap_types + gtd.targtype1_txt + gtd.multiple + gtd.iyear + gtd.property, data = top5Train, method = "class")
prp(CART7_region)

predictCART7_region = predict(CART7_region, newdata = top5Test, type = "class")
table(top5Test$gtd.gname, predictCART7_region)
(82+413+233+484+844)/nrow(top5Test)
#Accuracy about 85.88% with region

CART7_region$variable.importance

CART7 <- rpart(gtd.gname ~ gtd.nperps + gtd.mult_weap_types + gtd.targtype1_txt + gtd.multiple + gtd.iyear + gtd.property, data = top5Train, method = "class")
prp(CART7)
title(main = "CART Model 7 Decision Tree")
predictCART7 = predict(CART7, newdata = top5Test, type = "class")
table(top5Test$gtd.gname, predictCART7)
(0+152+109+288+638)/nrow(top5Test)
#Accuracy about 49.58% without region

CART7$variable.importance


#Can we improve these models with random forests?
set.seed(1000)
library(randomForest)

RF1 <- randomForest(gtd.gname ~ gtd.vicinity + gtd.success + gtd.suicide + gtd.property, data = top5Train)
predictRF1 = predict(RF1, newdata = top5Test)
table(top5Test$gtd.gname, predictRF1)
#Accuracy about 37.51%

vu1 <- varUsed(RF1, count = TRUE)
vu1sorted <- sort(vu1, decreasing = FALSE, index.return = TRUE)
dotchart(vu1sorted$x, names(RF1$forest$xlevels[vu1sorted$ix]))
#Chart show that whether or not there was property damage was selected for splitting the most. Property with success and vicinity are 
#significantly more important than suicide.

varImpPlot(RF1)
#Again property has significantly more average reduction in impurity than the other three variables. Interestingly, suicide has the 
#second highest. 

RF2 <- randomForest(gtd.gname ~ gtd.iyear + gtd.imonth + gtd.iday + gtd.extended + gtd.region_txt, data = top5Train)
predictRF2 = predict(RF2, newdata = top5Test)
table(top5Test$gtd.gname, predictRF2)
(99+340+192+482+845)/nrow(top5Test)
#Accuracy about 81.79%

vu2 <- varUsed(RF2, count = TRUE)
vu2sorted <- sort(vu2, decreasing = FALSE, index.return = TRUE)
dotchart(vu2sorted$x, names(RF2$forest$xlevels[vu2sorted$ix]))
#Here, the day and the month of the attack were selected for splits significantly more than year, region and whether or not the incident
#extended for more than 24 hours.

varImpPlot(RF2, main = "Average Reduction in Impurity")
#Region has by far the most average reduction in impurity

RF3 <- randomForest(gtd.gname ~ gtd.crit1 + gtd.crit2 + gtd.crit3 + gtd.multiple + gtd.doubtterr + gtd.attacktype1_txt, data = top5Train)
predictRF3 = predict(RF3, newdata = top5Test)                    
table(top5Test$gtd.gname, predictRF3)
(0+121+64+213+675)/nrow(top5Test)
#Accuracy about 44.82%

vu3 <- varUsed(RF3, count = TRUE)
vu3sorted <- sort(vu3, decreasing = FALSE, index.return = TRUE)
dotchart(vu3sorted$x, names(RF3$forest$xlevels[vu3sorted$ix]))
#The type of attack was selected for splitting far more than the other variables.

varImpPlot(RF3)
#Multiple and attack type have highest average reduction in impurity.

RF4 <- randomForest(gtd.gname ~ gtd.targtype1_txt + gtd.guncertain1 + gtd.claimed + gtd.weaptype1_txt + gtd.ishostkid, data = top5Train)
predictRF4 = predict(RF4, newdata = top5Test)
table(top5Test$gtd.gname, predictRF4)
(13+210+206+264+584)/nrow(top5Test)
#Accuracy about 53.34%

vu4 <- varUsed(RF4, count = TRUE)
vu4sorted <- sort(vu4, decreasing = FALSE, index.return = TRUE)
dotchart(vu4sorted$x, names(RF4$forest$xlevels[vu4sorted$ix]), main = "Number of Times Selected for Splits")
#Target type was selected the most for splitting

varImpPlot(RF4, main = "Average Reduction in Impurity")
#Target type has highest average reduction in impurity

RF5 <- randomForest(gtd.gname ~ gtd.mult_natlty + gtd.mult_targtypes + gtd.mult_weap_types + gtd.mult_attack_types, data = top5Train)
predictRF5 = predict(RF5, newdata = top5Test)
table(top5Test$gtd.gname, predictRF5)
(0+10+116+0+782)/nrow(top5Test)
#Accuracy about 37.93%

vu5 <- varUsed(RF5, count = TRUE)
vu5sorted <- sort(vu5, decreasing = FALSE, index.return = TRUE)
dotchart(vu5sorted$x, names(RF5$forest$xlevels[vu5sorted$ix]))
#Whether or not there were multiple attack types was selected for splitting significantly more than other variables

varImpPlot(RF5)
#Multiple weapon types have highest average reduction in impurity


RF6 <- randomForest(gtd.gname ~ gtd.nwoundus + gtd.nkillus + gtd.nperps + gtd.nperpcap, data = top5Train)
predictRF6 = predict(RF6, newdata = top5Test)
table(top5Test$gtd.gname, predictRF6)
(0+0+39+0+825)/nrow(top5Test)
#Accuracy about 36.09%

vu6 <- varUsed(RF6, count = TRUE)
vu6sorted <- sort(vu6, decreasing = FALSE, index.return = TRUE)
dotchart(vu6sorted$x, names(RF6$forest$xlevels[vu6sorted$ix]))
#The number of terrorists involved in the incident

varImpPlot(RF6)
#The number of terrorists involved in the incident

RF7 <- randomForest(gtd.gname ~ gtd.nperps + gtd.mult_weap_types + gtd.targtype1_txt + gtd.multiple + gtd.iyear + gtd.property, data = top5Train)
predictRF7 = predict(RF7, newdata = top5Test)
table(top5Test$gtd.gname, predictRF7)
(15+199+173+305+637)/nrow(top5Test)
#Accuracy about 55.51

vu7 <- varUsed(RF7, count = TRUE)
vu7sorted <- sort(vu7, decreasing = FALSE, index.return = TRUE)
dotchart(vu7sorted$x, names(RF7$forest$xlevels[vu7sorted$ix]), main = "Number of Times Selected for Splits")
#nperps

varImpPlot(RF7, main = "Average Reduction in Impurity")
#region


RF7_region <- randomForest(gtd.gname ~ gtd.region_txt + gtd.nperps + gtd.mult_weap_types + gtd.targtype1_txt + gtd.multiple + gtd.iyear + gtd.property, data = top5Train)
predictRF7_region = predict(RF7_region, newdata = top5Test)
table(top5Test$gtd.gname, predictRF7_region)
(122+428+271+472+845)/nrow(top5Test)
#Accuracy about 89.31% with region incorporated

vu7_region <- varUsed(RF7_region, count = TRUE)
vu7sorted_region <- sort(vu7_region, decreasing = FALSE, index.return = TRUE)
dotchart(vu7sorted_region$x, names(RF7_region$forest$xlevels[vu7sorted_region$ix]), main = "Number of Times Selected for Splits")
#nperps

varImpPlot(RF7_region, main = "Average Reduction in Impurity")
#region

#Bar graph with accuracy according to models
df_accuracy <- data.frame(Model = c("Baseline", "Model1", "Model2", "Model3", "Model4", "Model5", "Model6", "Model7", "Model7 with Region"),
                          CART_Accuracy = c(35.28, 37.64, 80.80, 45.15, 49.29, 37.93, 36.09, 49.58, 85.88),
                          RF_Accuracy = c(35.28, 37.51, 81.79, 44.82, 53.34, 37.93, 36.09, 55.51, 89.31))

library(ggplot2)
library(grid)
library(gridExtra)
CART_plot <- ggplot(aes(x = Model, y = CART_Accuracy, fill = Model), data = df_accuracy) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = CART_Accuracy), vjust = 1.5)

RF_plot <- ggplot(aes(x = Model, y = RF_Accuracy, fill = Model), data = df_accuracy) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = RF_Accuracy), vjust = 1.5)

grid.arrange(CART_plot, RF_plot, ncol = 1, main = "CART and Random Forest Accuracy")

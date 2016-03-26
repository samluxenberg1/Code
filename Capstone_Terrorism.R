#Captstone Project
#Global Terrorism
#Data is from Global Terrorism Database at the University of Maryland

#Research Question
#Given various characteristics of terrorist attacks, is it possible to accurately predict the perpetrators of 
#an attack? If so, this model could help identify behaviors of specific terrorist groups and what typical 
#consequences are of of their attacks. 

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
terror <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/Global_Terror_Data.csv')
terrorism <- terror

#Variables  to consider

#Get rid of variables we won't use
terrorism$approxdate <- NULL
terrorism$country <- NULL
terrorism$region <- NULL
terrorism$city<- NULL
terrorism$specificity<- NULL
terrorism$location<- NULL
terrorism$summary<- NULL
terrorism$alternative<- NULL
terrorism$alternative_txt<- NULL
#terrorism$attacktype1<- NULL

#terrorism$targtype1<- NULL
#terrorism$targsubtype1<- NULL
terrorism$corp1<- NULL
terrorism$corp2<- NULL
terrorism$corp3<- NULL
#terrorism$target1<- NULL
#terrorism$target2<- NULL
#terrorism$target3<- NULL
#terrorism$natlty1<- NULL
#terrorism$gsubname<- NULL
#terrorism$gsubname2<- NULL
#terrorism$gsubname3<- NULL
terrorism$motive<- NULL
terrorism$weapdetail<- NULL
terrorism$propextent<- NULL
terrorism$propvalue<- NULL
terrorism$propcomment<- NULL
terrorism$ransomamtus<- NULL
terrorism$ransompaidus<- NULL
terrorism$hostkidoutcome<- NULL
terrorism$addnotes<- NULL
terrorism$scite1<- NULL
terrorism$scite2<- NULL
terrorism$scite3<- NULL
terrorism$dbsource<- NULL
terrorism$INT_LOG<- NULL
terrorism$INT_IDEO<- NULL
terrorism$INT_MISC<- NULL
terrorism$INT_ANY<- NULL
terrorism$related<- NULL

#Newly created variables
terrorism$mult_attack_types <- 0
terrorism$mult_attack_types <- ifelse(is.na(terrorism$attacktype2), 0, 1)
terrorism$mult_targtypes <- 0
terrorism$mult_targtypes <- ifelse(is.na(terrorism$targtype2), 0, 1)
terrorism$mult_weap_types <- ifelse(is.na(terrorism$weaptype2), 0, 1)
terrorism$mult_natlty <- ifelse(is.na(terrorism$natlty2), 0, 1)

#Edited variables
terrorism$nperps <- ifelse(terrorism$nperps == "Unknown", -99, terrorism$nperps)
terrorism$nperps <- as.numeric(terrorism$nperps)
terrorism$nperps <- ifelse(is.na(terrorism$nperps), -99, terrorism$nperps)
terrorism$nperpcap <- ifelse(terrorism$nperpcap == -9, -99, terrorism$nperpcap)
terrorism$nperpcap <- ifelse(is.na(terrorism$nperpcap), 0, terrorism$nperpcap)
terrorism$latitude <- as.numeric(terrorism$latitude)
terrorism$claimed <- ifelse(is.na(terrorism$claimed), 0, 1)
terrorism$compclaim <- ifelse(is.na(terrorism$claim2), 0, 1)
terrorism$nkill <- ifelse(is.na(terrorism$nkill), 2.256, terrorism$nkill) #mean for NAs
terrorism$nkillter <- ifelse(is.na(terrorism$nkillter), .28, terrorism$nkillter) #mean for NAs
terrorism$nwound <- ifelse(is.na(terrorism$nwound), 3.108, terrorism$nwound) #mean for NAs
terrorism$nwoundte <- ifelse(is.na(terrorism$nwoundte), .04, terrorism$nwoundte) #mean for NAs
terrorism$nhostkid <- ifelse(is.na(terrorism$nhostkid), 0, terrorism$nhostkid)
terrorism$nhostkidus <- ifelse(is.na(terrorism$nhostkidus), 0, terrorism$nhostkidus)
terrorism$ransomamt <- ifelse(is.na(terrorism$ransomamt), 0, terrorism$ransomamt)
terrorism$nreleased <- ifelse(is.na(terrorism$nreleased), 0, terrorism$nreleased)

#Remove variables that also have a text version
terrorism$attacktype2<- NULL
terrorism$attacktype3<- NULL
terrorism$targtype2<- NULL
terrorism$targtype3<- NULL
terrorism$targsubtype2<- NULL
terrorism$targsubtype3<- NULL
terrorism$natlty2<- NULL
terrorism$natlty3<- NULL
terrorism$guncertain1<-NULL
terrorism$guncertain2<-NULL
terrorism$guncertain3<-NULL
terrorism$claimmode<- NULL
terrorism$claimmode2<- NULL
terrorism$claimmode3<- NULL
terrorism$weaptype1<- NULL
terrorism$weaptype2<- NULL
terrorism$weaptype3<- NULL
terrorism$weaptype4<- NULL
terrorism$weapsubtype1<- NULL
terrorism$weapsubtype2<- NULL
terrorism$weapsubtype3<- NULL
terrorism$weapsubtype4<- NULL
terrorism$nkillus<-NULL
terrorism$nwoundus<-NULL
terrorism$ndays<-NULL
terrorism$nhours<-NULL
terrorism$npers<-NULL
terrorism$claim2<-NULL
terrorism$claim3<-NULL
terrorism$ransompaid<-NULL

#Analysis

#Correlations
cor.test(terrorism$nkill, terrorism$nkillter)
#Weak positive correlation: violence begets violence
cor.test(terrorism$nkill, terrorism$property)
#zero linear correlation between amount of property damage and number killed
cor.test(terrorism$nkill, terrorism$nhostkid) # little correlation
cor.test(terrorism$nkill, terrorism$ransomamt) #little correlation
cor.test(terrorism$nkillter, terrorism$property) #little correlation
cor.test(terrorism$nkillter, terrorism$nhostkid) #little correlation
cor.test(terrorism$nkillter, terrorism$ransomamt) #little correlation
errorism$nwound, terrorism$nwoundte, terrorism$property, terrorism$nhostkid, terrorism$ransomamt)

df_cor <- data.frame(terrorism$nkill, terrorism$nkillter, terrorism$nreleased, terrorism$nperps, terrorism$nperpcap, terrorism$success,terrorism$property, terrorism$nhostkid, terrorism$ransomamt)
cor(df_cor)
library(ggplot2)
ggplot(aes(x = nkill), data = terrorism) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0, 100))
#nkill is skewed
ggplot(aes(x = nkill), data = terrorism) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0, 40))
ggplot(aes(x = nkill))

library(caTools)
set.seed(123)
spl <- sample.split(terrorism$gname, SplitRatio = .7)
Train <- subset(terrorism, spl == TRUE)
Test <- subset(terrorism, spl == FALSE)

library(rpart)
library(rpart.plot)

#5 Most Common groups
head(sort(table(terrorism$gname), decreasing = TRUE), n = 10)
toporgsTrain <- subset(Train, Train$gname == "Unknown" | Train$gname == "Shining Path (SL)" | Train$gname == "Farabundo Marti National Liberation Front (FMLN)" | Train$gname == "Taliban" | Train$gname == "Irish Republican Army (IRA)")

| Train$gname == "Revolutionary Armed Forces of Colombia (FARC)" | Train$gname == "Basque Fatherland and Freedom (ETA)" | Train$gname == "New People's Army (NPA)" | Train$gname == "Liberation Tigers of Tamil Eelam (LTTE)" | Train$gname == "Communist Party of India - Maoist (CPI-Maoist)

TrainCART <- rpart(gname ~ ., data = toporgsTrain, method = "class"



#propextent
#gtd$propextent <- ifelse(gtd$property == 0, 0, gtd$propextent)
#gtd$propextent <- ifelse(gtd$property == -9, -9, gtd$propextent)
#gtd$propextent <- ifelse(gtd$property == 1 & (is.na(gtd$propextent) | gtd$propextent == 4), -9, gtd$propextent)

#propextent_txt
#gtd$propextent_txt <- ifelse(gtd$property == 0, "None", gtd$propextent_txt)
#gtd$propextent_txt <- ifelse(gtd$property == -9, "Unknown", gtd$propextent_txt)
#gtd$propextent_txt <- ifelse(gtd$property == 1 & is.na(gtd$propextent), "Unknown", gtd$propextent_txt)

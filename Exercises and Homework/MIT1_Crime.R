#MIT Analytics Edge Week 1 - Crime
setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
mvt <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/mvtWeek1.csv')
str(mvt)
summary(mvt)
mvt$Date[2000]
#Convert into date object
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(mvt$Date)
head(DateConvert)
summary(DateConvert)
mvt$Month = month(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
min(table(mvt$Month))
table(mvt$Weekday)
by(mvt$Arrest, mvt$Month, summary)
table(mvt$Arrest, mvt$Month)

#histogram
library(ggplot2)
ggplot(aes(x = Date), data = mvt) +
  geom_histogram()

ggplot(aes(x = Arrest, y = Date), data = mvt) +
  geom_boxplot()

table(year(mvt$Date), mvt$Arrest)
table(mvt$Arrest, mvt$Year)

#top 5
sort(table(mvt$LocationDescription))
Top5 <- subset(mvt, LocationDescription == "STREET"| LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | 
                 LocationDescription == "ALLEY" | LocationDescription == "GAS STATION" |
                 LocationDescription == "DRIVEWAY - RESIDENTIAL")

#Alt. Method for Top5
#TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
#Top5 = subset(mvt, LocationDescription %in% TopLocations)

table(Top5$LocationDescription)
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$Weekday, Top5$LocationDescription)

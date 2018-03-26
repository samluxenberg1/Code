#MIT Analytics Edge Week 1 - Demographics and Unemployment in the US
setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
CPS <- read.csv("C:/Users/Samuel/Documents/Slide Rule/Data/CPSData.csv")
str(CPS)
summary(CPS)
table(CPS$Industry)
sort(table(CPS$Region))
sort(table(CPS$State))
table(CPS$Citizenship)

#For which races are there at least 250 interviewees of Hispanic ethnicity?
table(subset(CPS, Hispanic == 1)$Race)
#OR
table(CPS$Race, CPS$Hispanic)

#Try to find pattern of missing values
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))  

#How many states had all interviewees living in a non-metropolitan area?
table(CPS$State, is.na(CPS$MetroAreaCode))

table(CPS$Region, is.na(CPS$MetroAreaCode))

#Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap <- read.csv("C:/Users/Samuel/Documents/Slide Rule/Data/MetroAreaCodes.csv")
CountryMap <- read.csv("C:/Users/Samuel/Documents/Slide Rule/Data/CountryCodes.csv")

str(MetroAreaMap)
str(CountryMap)

CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

table(CPS$MetroArea.x)
sort(tapply(CPS$Hispanic, CPS$MetroArea.x, mean))

#Determine number of metropolitan areas from which at least 20% are Asian
sort(tapply(CPS$Race == "Asian", CPS$MetroArea.x, mean))

#Determine which metropolitan area has the smallest proportion of interviewees without high school diploma
#(excluding ages <= 14)
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea.x, mean, na.rm = TRUE))

CPS <- merge(CPS, CountryMap, by.x = "CountryOfBirthCode", by.y = "Code", all.x = TRUE)

#What proportion of interviewees from New York-Northern New Jersey-Long Island, NY-NJ-PA metropolitan area  
#have a country of birth that is not the United States? Don't include people with missing country of birth.

table(CPS$MetroArea.x == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

table(CPS$MetroArea.x == "Boston-Cambridge-Quincy, MA-NH", CPS$Country == "India")
table(CPS$MetroArea.x == "Minneapolis-St Paul-Bloomington, MN-WI", CPS$Country == "India")
table(CPS$MetroArea.x == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country == "India")
table(CPS$MetroArea.x == "Washington-Arlington-Alexandria, DC-VA-MD-WV", CPS$Country == "India")

table(CPS$MetroArea.x == "Boston-Cambridge-Quincy, MA-NH", CPS$Country == "Brazil")
table(CPS$MetroArea.x == "Minneapolis-St Paul-Bloomington, MN-WI", CPS$Country == "Brazil")
table(CPS$MetroArea.x == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country == "Brazil")
table(CPS$MetroArea.x == "Washington-Arlington-Alexandria, DC-VA-MD-WV", CPS$Country == "Brazil")

table(CPS$MetroArea.x == "Boston-Cambridge-Quincy, MA-NH", CPS$Country == "Somalia")
table(CPS$MetroArea.x == "Minneapolis-St Paul-Bloomington, MN-WI", CPS$Country == "Somalia")
table(CPS$MetroArea.x == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country == "Somalia")
table(CPS$MetroArea.x == "Washington-Arlington-Alexandria, DC-VA-MD-WV", CPS$Country == "Somalia")

#Alternative Method
sort(tapply(CPS$Country == "India", CPS$MetroArea.x, sum, na.rm = TRUE))

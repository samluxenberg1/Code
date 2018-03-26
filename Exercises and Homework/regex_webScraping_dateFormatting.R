#Regular Expressions and Dates

#Regular Expressions
grep("stalin", c("stalin", "stalingrad"), value = T)
grep("stalin\\b",c("stalin","stalingrad"),value = T)

#gsub(pattern, replacement, x)
author <- "By Rolf Fredheim"
gsub("By ", "", author)
gsub("Rolf Fredheim", "Tom", author)

fruits <- c("apples and oranges and pears and bananas", "pineapples and mangos and guavas")
str_split(fruits, " and ")

annoyingString <- "\n  something  HERE  \t\t\t"
nchar(annoyingString)
str_trim(annoyingString)

#formatting dates
require(lubridate)
as.Date("2014-01-31")
date <- as.Date("2014-01-31")
str(date)

date + years(1)
date - months(6)
date - days(1110)

#full way to enter dates
as.Date("2014-01-31", "%Y-%m-%d")
#%Y = 4 digits, 2004
#%y = 2 digits, 04
#%m = month
#%d = day
#%b = month in characters

date <- "04 March 2014"
as.Date(date, "%d %b %Y")

#Lubridate makes everything easier
date <- "04 March 2014"
dmy(date)

time <- "04 March 2014 16:10:00"
dmy_hms(time, tz = "GMT")
time2 <- "2014/03/01 07:44:22"
ymd_hms(time2, tz = "GMT")

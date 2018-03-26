#Web Scraping 1

require (ggplot2)
clubs <- c("Tottenham","Arsenal","Liverpool",
           "Everton","ManU","ManC","Chelsea")
nPages <- c(23.3,68.4,78.9,35.5,102,90.5,110)
df <- data.frame(clubs,nPages)
df
ggplot(df, aes(clubs, nPages, fill = clubs)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_bw(base_size = 70)

var=123
paste("url", var, sep="")
paste("url", var, sep = " ")
paste0("url", var, sep = "")
paste("url", rep(var, 3), sep = "_")
paste(rep("url", 3), var, sep = "_")
var = c(123, 421)
paste(var, collapse = "_")

#With a URL
var = 201401
paste("http://stats.grok.se/json/en/",var,"/web_scraping")
paste("http://stats.grok.se/json/en/",var,"/web_scraping",sep="")
a = "test"
b = "scrape"
c = 94
#merge variables a,b,c into a string, separated by an underscore ("_")
paste(a,b,c,sep="_")
#merge variables a,b,c into a string without any separating character
paste(a,b,c,sep="")
#print the letter 'a' followed by the numbers 1:10, without a separating character
paste('a', 1:10, sep="")

var=201401

url=paste("http://stats.grok.se/json/en/",var,"/web_scraping",sep="")

url
browseURL(url)

#Fetch data
var=201401
url=paste("http://stats.grok.se/json/en/",var,"/web_scraping",sep="")
raw.data <- readLines(url, warn="F")
raw.data

require(rjson)
rd<-fromJSON(raw.data)
rd

rd.views<-rd$daily_views
rd.views

rd.views<-unlist(rd.views)
df<-as.data.frame(rd.views)
df

#Put it together
rd<-fromJSON(readLines(url, warn = "F"))
rd.views <- rd$daily_views
df<-as.data.frame(unlist(rd.views))

#Plot it
require(ggplot2)
require(lubridate)
df$date<-as.Date(rownames(df))
colnames(df)<-c("views", "date")
ggplot(aes(x = date, y = views), data = df) +
  geom_line() +
  geom_smooth() +
  theme_bw(base_size = 20)

#Sochi Games
var = 201402
url=paste("http://stats.grok.se/json/en/",var,"/Sochi%20games",sep="")
raw.data <- readLines(url, warn="F")
raw.data

rd <- fromJSON(raw.data)
rd

rd.views <- rd$daily_views
rd.views <- unlist(rd.views)
df <- as.data.frame(rd.views)

rd <- fromJSON(readLines(url, warn = "F"))
rd.views <- rd$daily_views
df <- as.data.frame(unlist(rd.views))
df$date <- as.Date(rownames(df))
colnames(df) <- c("views", "date")
p1 <- ggplot(aes(x = date, y = views), data = df) +
  geom_line() +
  geom_smooth() +
  theme_bw(base_size = 20)

#Russian
url=paste("http://stats.grok.se/json/ru/",var,"/Sochi%20games",sep="")
raw.data.r <- readLines(url, warn="F")
raw.data.r

rd.r <- fromJSON(raw.data.r)
rd.r

rd.r.views <- rd.r$daily_views
rd.r.views <- unlist(rd.r.views)
df.r <- as.data.frame(rd.r.views)

rd.r <- fromJSON(readLines(url, warn = "F"))
rd.r.views <- rd.r$daily_views
df.r <- as.data.frame(unlist(rd.r.views))
df.r$date <- as.Date(rownames(df.r))
colnames(df.r) <- c("views", "date")
p2 <- ggplot(aes(x = date, y = views), data = df.r) +
  geom_line() +
  geom_smooth() +
  theme_bw(base_size = 20)

library(gridExtra)
grid.arrange(p1,p2)


#Loops
plus0ne <- function(x){
  return(x+1)
}
plus0ne
plus0ne2 <- function(num){
  return(num+1)
}
plus0ne2
plus0ne(8)
plus0ne2(10)
plus0ne2(num = 5)

for (number in 1:5){
  print(number)
  
}

#Looping over functions
a <- seq(1, 5, 1)
a
class(a)
a<-c(1,2,3,4,5)
class(a)
for (value in a){
  print(
    plus0ne(value)
  )
  
}

list0fNumbers <- c(1,2,3,4,5)
for (number in list0fNumbers){
  print(
    number + 1
  )
  
}

for (i in 1:length(a)){
  print(
    plus0ne(a[i])
  )
}
#vectors
a<-c(1,2,3,4,5) #least flexible, fastest
a+1
plus0ne(a)#quite flexible
sapply(a, plus0ne)#can be used in all sorts of situations, slow - similar to a loop,
#better if colleting an output

#URLs
for (month in 1:12){
  print(paste(2014, month, sep = ""))
}

dates = NULL
for (month in 1:9){
  date = (paste(2012, 0, month, sep = ""))
  dates = c(dates, date)
}
  for (month in 10:12){
    date = (paste(2012, month, sep = ""))
    dates = c(dates, date)
  }
print(as.numeric(dates))

#add another variable
for (year in 2012:2013){
  for(month in 1:9){
    print(paste("http://stats.grok.se/json/en/",year, 0, month, "/web_scraping", sep = ""))
  }
  for (month in 10:12){
    print(paste("http://stats.grok.se/json/en/", year, month, "/web_scraping", sep = ""))
  }
}

#Loop Tasks
for (i in 1:1000){
  print(i)
}

#sum first 1000 numbers
sum1000 <- 0
for (i in 1:1000){
  sum1000 = sum1000 + i
}

#write function that takes input number and divides this number by 2
divby2 <- function(x){
  x/2
}
divby2(52)

#write function that returns 99 no matter the input
num99 <- function(x){
  99
  
}
num99(12)

#write function that takes two variables and sums them
sum2 <- function(x, y){
  x + y
}
sum2(2,10)

#where I left off: http://fredheir.github.io/WebScraping/Lecture1/p1.html#/50

#Make an application which takes a Wikipedia page and returns a plot for the month 201312
var = 201312
url=paste("http://stats.grok.se/json/en/",var,"/web%20scraping",sep="")
raw.data <- readLines(url, warn="F")
raw.data

rd <- fromJSON(raw.data)
rd

rd.views <- rd$daily_views
rd.views <- unlist(rd.views)
df <- as.data.frame(rd.views)

rd <- fromJSON(readLines(url, warn = "F"))
rd.views <- rd$daily_views
df <- as.data.frame(unlist(rd.views))
df$date <- as.Date(rownames(df))
colnames(df) <- c("views", "date")
p1 <- ggplot(aes(x = date, y = views), data = df) +
  geom_line() +
  geom_smooth() +
  theme_bw(base_size = 20)
 
 for(month in 1:9){
    url[month] = paste("http://stats.grok.se/json/en/",2013, 0, month, "/web_scraping", sep = "")
    raw.data[month] <- readLines(url[month], warn = "F")
    rd[month] <- fromJSON(raw.data[month])
    rd.views[month] <- rd[month]$daily_views
    #rd.views[month] <- unlist(rd.views[month])
    #df[month] <- as.data.frame(rd.views[month])
    #rd[month] <- fromJSON(readLines(url[month], warn = "F"))
    #rd.views[month] <- rd[month]$dail_views
    #df[month] <- as.data.frame(unlist(rd.views[month]))
    #df[month]$date <- as.Date(rownames(df[month]))
    #colnames(df[month]) <- c("views", "date")
  }
  for (month in 10:12){
    print(paste("http://stats.grok.se/json/en/", year, month, "/web_scraping", sep = ""))
  }
require(rjson)


#Getting started with functions
getData <- function(url){
  raw.data <- readLines(url, warn = "F")
  rd <-fromJSON(raw.data)
  rd.views <- rd$daily_views
  rd.views <- unlist(rd.views)
  rd <- as.data.frame(rd.views)
  rd$date <- rownames(rd)
  rownames(rd) <- NULL
  return(rd)
}

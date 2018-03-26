#Web Scraping 2
sillySimulation <- function(){
  x1 <- runif(500, 80, 100)
  x2 <- runif(500, 0, 100)
  v1 <- c(x1, x2)
  
  x3 <- runif(1000, 0, 100)
  df <- data.frame(v1, x3)
  require(ggplot2)
  
  print(ggplot(aes(v1, x3), df) + geom_point() + ggtitle("simulation of some sort"))
}
library(ggplot2)
library(lubridate)
#Inserting variables
desperateTimes <- function(name, gender = "m", degree){
  if (gender == "m"){
    pronoun = "his"
  } else {
    pronoun = "her"
  }
  print(paste0(name, " is struggling to finish ", pronoun," ", degree, " on time. Time remaining: 6 months"))
}
desperateTimes(name = "Tanya", gender = "f", degree = "MPhil")

deadline = as.Date("2015-09-01")
daysLeft <- deadline-Sys.Date()
totDays <- deadline-as.Date("2011-10-01")
print(daysLeft)

print(paste0("Rolf is struggling to finish his PhD on time. Time remaining: ", 
             as.numeric(daysLeft), " days"))

print(paste0("Percentage to go: ", round(as.numeric(daysLeft)/as.numeric(totDays)*100, 2)))

df <- data.frame(days = c(daysLeft, totDays - daysLeft), lab = c("to go", "completed"))
ggplot(df, aes(1, days, fill = lab)) + geom_bar(stat = "identity", position = "fill")

#put all this code into one function and can forget about it
timeToWorry <- function(){
  require(lubridate)
  deadline = as.Date("2015-09-01")
  daysLeft <- deadline - Sys.Date()
  totDays <- deadline - as.Date("2011-10-01")
  print(daysLeft)
  print(paste0("Rolf is struggling to finish his PhD on time. Time remaining: ", as.numeric(daysLeft), " days"))
  print(paste0("Percentage to go: ", round(as.numeric(daysLeft)/as.numeric(totDays)*100, 2)))
  df <- data.frame(days = c(daysLeft, totDays - daysLeft), lab = c("to go", "completed"))
  ggplot(df, aes(1, days, fill = lab)) + geom_bar(stat = "identity", position = "fill")
}


#From part 1
require(rjson)
url <-  "http://stats.grok.se/json/en/201201/web_scraping"
raw.data <- readLines(url, warn="F") 
rd  <- fromJSON(raw.data)
rd.views <- rd$daily_views 
rd.views <- unlist(rd.views)
rd <- as.data.frame(rd.views)
rd$date <- rownames(rd)
rownames(rd) <- NULL

getData <- function(url){
  require(rjson)
  raw.data <- readLines(url, warn="F") 
  rd  <- fromJSON(raw.data)
  rd.views <- rd$daily_views 
  rd.views <- unlist(rd.views)
  rd <- as.data.frame(rd.views)
  rd$date <- rownames(rd)
  rownames(rd) <- NULL
  rd$date <- as.Date(rd$date)
  return(rd)
}

getData("http://stats.grok.se/json/en/201201/web_scraping")

getURLs <- function(y1, y2, term){
  root = "http://stats.grok.se/json/en/"
  urls <- NULL
  for (year in y1:y2){
    for (month in 1:9){
      urls <- c(urls, (paste(root, year, 0, month, "/", term, sep = "")))
    }
    for (month in 10:12){
      urls <- c(urls, (paste(root, year, month, "/", term, sep = "")))
    }
  } 
  return(urls)
}

urls <- getURLs(y1 = 2013, y2 = 2014, term = "Euromaidan")
results = NULL
for (url in urls){
  results <- rbind(results, getData(url))
}
head(results)
ggplot(aes(x = date, y = rd.views), data = tail(results, 100)) + geom_line()

#HTML
#HTML tags come in pairs and surrounded by <>
#<hl>MY HEADING</hl>
#<html>: starts html codeF
#<head>: contains metadata etc.
#<script>: e.g. javascript to be loaded
#<style>: css code
#<meta>: denotes document properties, e.g. author, keywords
#<title>
#<body>
#<div>, <span>: these are used to break up the document into sections and boxes
#<h1>, <h2>, <h3>, <h4>, <h5>: different levels of heading
#<p>: paragraph
#<br>: line break
#and others: <a>, <ul>, <tbody>, <th>, <td>, <u1>

#XML parser
#Pareses an XML or HTML file or string containing XML/HTML content, and generates an R structure 
#representing the XML/HTML tree
require(RCurl)
require(XML)

url <- "https://en.wikipedia.org/wiki/Euromaidan"
SOURCE <-  getURL(url) #Download the page
substring(SOURCE< 1, 200)
PARSED <- htmlParse(SOURCE) #format the html code d

#Accessing HTML elements in R
#Use XPath expressions to extract elements from HTML
xpathSApply(PARSED, "//h1")
xpathSApply(PARSED, "//h1", xmlValue)

#Other headings
xpathSApply(PARSED, "//h3", xmlValue)

length(xpathSApply(PARSED, "//a/@href"))  

#CSS allows us to make better selections, by latching onto tags
#Xpath allows us to move up and down the html tree structure

#Content of references
head(xpathSApply(PARSED,
                 "//span[@class='citation news']",xmlValue))
#use function from part 1 to test these work
links <- (xpathSApply(PARSED, "//span[@class='citation news']/a/@href"))
browseURL(links[2])

#XPath is a language for querying XML
#element in quotes is an XPath expression
head(xpathSApply(PARSED, "//span[@class='citation news']/a//@href"))

#Fundamental XPath syntax
#'/' select from the root
#'//' select anywhere in the document
#'@' select attributes. use in square brackets
#in the above example, we select all elements of 'span'...which have an attribute "class" of the value
#"citation news"... then we select all links...and return all attributes labeled "href" (the urls)
#Like in R we use brackets to make selections
head(xpathSApply(PARSED, "//span[@class='citation news'][1]/a/@href"))

#Wildcards
#'*' select any node or tag
#'@*' selects any attribute (used to define nodes)
(xpathSApply(PARSED, "//*[@class='citation news'][2]/a/@href"))
(xpathSApply(PARSED, "//span[@class='citation news'][2]/a/@*"))

#You can use functions, e.g. for partial matches -> useful if there are subtle variations within or between pages
head(xpathSApply(PARSED, "//span[starts-with(@class,'citation')][2]/a/@href"))
head(xpathSApply(PARSED, "//span[contains(@class,'citation')][2]/a/@href"))

#Example - BBC
url <- "http://www.bbc.co.uk/news/world-europe-26333587"
SOURCE <-  getURL(url,encoding="UTF-8") # Specify encoding when dealing with non-latin characters
PARSED <- htmlParse(SOURCE)
(xpathSApply(PARSED, "//span[@class='story-header']"))
(xpathSApply(PARSED, "//h1[@class='story-header']",xmlValue))
xpathSApply(PARSED, "//span[@class='date']",xmlValue)








url <- "http://www.theguardian.com/commentisfree/2014/feb/25/how-much-cost-growers-bananas-68p-per-kilo"
url
SOURCE <-  getURL(url,encoding="UTF-8")
SOURCE
PARSED <- htmlParse(SOURCE)
PARSED
xpathSApply(PARSED, "//h1[contains(@itemprop, 'headline')]", xmlValue)
xpathSApply(PARSED, "//a[@class='contributor']", xmlValue)

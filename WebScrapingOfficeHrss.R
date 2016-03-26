#Office Hours - Scraping Part 1

library(rvest)

#Scraper 1
url = "http://nuforc.org/webreports/ndxe201507.html"
pg <- html(url)
table <- pg %>% 
  html_nodes("table") %>%
  html_table()
str(table)
View(table)

#Scraper 2
url2 <- "http://carzoom.in/car-specification/"
pg2 <- html(url2)
nodes2 <- html_nodes(pg2, ".car-model li a")
title2 <- html_text(nodes2)
links2 <- html_attr(nodes2, "href")

#Scraper 3
url3 <- "http://nhrdf.org/en-us/AreaAndProductiionReport"
pg3 <- html(url3)
#more difficult--has hidden forms

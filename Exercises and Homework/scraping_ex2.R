library(rvest)

url <- "https://news.ycombinator.com/"
pg <- html(url)
nodes <-html_nodes(pg, ".athing a") 
title <- html_text(nodes)
links <- html_attr(nodes, "href")

nodes2 <- html_nodes(pg, ".subtext a~ a+ a , .athing a")
title <- html_text(nodes2)
links <- html_attr(nodes2, "href")
comments <- html_attr(nodes2, )

url2<-"http://www.stat.iastate.edu/people/faculty/"
html(url2) %>%
  html_nodes("#content a") %>%
  html_attr(name = "href")
nodes3<-html(pg2, "#contents a")
hfrefs <- html_attr(nodes3, name = "href")

url4 <- "http://www.math.iastate.edu/Directories/Faculty.html"
html(url4) %>%
  html_nodes("tr+ tr td , td+ td , #container a") %>%
  html_attr(name = "href") %>%
  html_attr(name = "class")

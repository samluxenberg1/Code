#Data Wrangling

library(reshape2)
library(ggplot2)
#library(dplyr)
library(data.table)
library(Lahman)

#Data Wrangling
#Tidy Data

#Messy Data
pew <- read.delim(
  file = "http://stat405.had.co.nz/data/pew.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = F
)
pew_tidy <- melt(
  data = pew, 
  id = "religion",
  variable.name = "income",
  value.name = "frequency"
)

tb <- read.csv(
  file = "http://stat405.had.co.nz/data/tb.csv",
  header = TRUE, 
  stringsAsFactors = FALSE
)
#Set column 'new_sp' to NULL and clean up column names
tb$new_sp = NULL
names(tb) <- gsub("new_sp_", "", names(tb))

#Use na.rm = TRUE to remove missing observations
tb_tidy <- melt(
  data = tb,
  id = c("iso2", "year"),
  variable.name = "gender_age",
  value.name = "cases",
  na.rm = TRUE
)

#Split gender_age into gender and age group
library(plyr)
tb_tidy <- mutate(tb_tidy,
                  gender = sub("^([m|f])(.*)$", "\\1", gender_age),
                  age = sub("^([m|f])(.*)$", "\\2", gender_age),
                  gender_age = NULL
)
tb_tidy <- tb_tidy[c('iso2', 'year', 'gender', 'age', 'cases')]
#One more step: assign age groups to more meaningful groups e.g. 04 is 0-4

weather <- read.delim(
  file = "http://stat405.had.co.nz/data/weather.txt",
  stringsAsFactors = FALSE
)
weather_tidy <- melt(
  data = weather,
  id = 1:4,
  variable.name = "day",
  value.name = "temperature",
  na.rm = TRUE
)
weather_tidy <- mutate(weather_tidy,
                       day = sub("^d", "", day)
                       )
weather_tidy2 <- dcast(
  data = weather_tidy,
  formula = id + year + month + day ~ element,
  value.var = "temperature"
)

#Bilboards
billboards <- read.csv(
  file = "http://stat405.had.co.nz/data/billboard.csv",
  stringsAsFactors = FALSE
)
names(billboards) <- gsub("\\.", "_", names(billboards))

billboards <- mutate(billboards,
                     artist_inverted = iconv(artist_inverted, "MAC", "UTF-8"),
)  
billboards_tidy <- melt(
  data = billboards,
  id = 1:7,
  variable.name = "week",
  value.name = "rank",
  na.rm = TRUE
)
billboards_tidy <- mutate(billboards_tidy, 
                          week = as.numeric(gsub("^x([[:digit:]]+).*", "\\1", week))
                          )


#Summarize
bnames2 <- read.csv("C:/Users/Samuel/Documents/Slide Rule/Data/bnames2.csv.bz2")
greg <- subset(bnames2, name == "Greg")
ggplot(aes(x = year, y = prop), data = greg) +
  geom_line()
michelle <- subset(bnames2, name == "Michelle")
ggplot(aes(x = year, y = prop), data = michelle) +
  geom_point()
ggplot(aes(x = year, y = prop), data = michelle) +
  geom_line(aes(color = sex))
greg_soundex <- greg$soundex[1]
greg_like <- subset(bnames2, soundex == greg_soundex)
ggplot(aes(x = year, y = prop), data = greg_like) +
  geom_point(aes(color = name))

#Track popularity of specific soundex--Need to summarize data by aggregating values
#of prop across all names with the same soundex.
#Subset, mutate, arrange, summarize, join, group

#Explore the following
#In which year was your name most popular (1900)? Least popular (1985)?
sam <- subset(bnames2, name == "Sam")
sam_max <- subset(sam, prop == max(prop))
sam_min <- subset(sam, prop == min(prop))

#Reorder the data frame containing your name from highest to lowest popularity
arrange(sam, desc(prop))

#Add a new column that gives the number of babies per thousand with your name
sam_1 <- summarise(sam,
                   year = year,
                   name = name,
                   prop = prop,
                   sex = sex,
                   soundex = soundex,
                   bpt = prop * 1000
          )
births <- read.csv("C:/Users/Samuel/Documents/Slide Rule/Data/births.csv")
ggplot(aes(x = year, y = births), data = births) +
  geom_line(aes(color = sex))

bnames2_b <- join(bnames2, births, by = c("sex", "year"))
greg <- subset(bnames2_b, name == "Greg")
greg <- mutate(greg, tot = prop * births)
ggplot(aes(x = year, y = tot), data = greg) +
  geom_line()

#What is the most popular name in the US across all these years?
#Use Split-Apply-Combine strategy
greg_tot <- summarise(greg, tot = sum(prop * births))
#Split data by name
pieces <- split(bnames2_b, bnames2_b$name)

#Apply
results <- vector("list", length(pieces))
for (i in seq_along(pieces)){
  results[[i]] <- summarise(pieces[[i]],
                            name = name[1],
                            tot = sum(prop * births)
                            )
}

#Combine
result <- do.call("rbind", results)

most_pop_name <- arrange(result, desc(tot))[1, "name"]

#Split-Apply-Combine: 1
bnames2_b = mutate(bnames2_b, tot = prop * births)
result <- aggregate(formula = tot ~ name, data = bnames2_b, FUN = sum)
#In aggregate, tot is to be split into groups according to name
#What is the most popular name by gender?
result2 <- aggregate(formula = tot ~ name + sex, data = bnames2_b, FUN = sum,
                     subset = (year >= 2000))
most_pop_boy <- arrange(subset(result2, sex == "boy"), desc(tot))[1, "name"]
most_pop_girl <- arrange(subset(result2, sex == "girl"), desc(tot))[1, "name"]

#Been using split-apply-combine for data frames. Can use this for other data structures too.
#Extract components by name from a list
#Compute mean across rows/columns of a matrix
#Apply a function across multiple sets of arguments

#apply: applies a function to each row or column of a matrix
m <- matrix(c(1:10, 11:20), nrow = 10, ncol = 2)
apply(m, 1, sum) #1 refers to row, 2 referrs to column
apply(m, 2, sum)
apply(m, 1, mean)
apply(m, 2, mean)

#lapply: applies a function to each element of a list
my_list <- list(a = 1:10, b = 2:20)
lapply(my_list, mean)

#sapply: more user-friendly version of lapply and will return a list or matrix where appropriate
x <- sapply(my_list, mean)

#mapply: can be thought of multivariate version of sapply. it applies a function to all corresponding 
#elements of each argument
list_1 <- list(a = c(1:10), b = c(11:20))
list_2 <- list(c = c(21:30), d = c(31:40))
mapply(sum, list_1$a, list_1$b, list_2$c, list_2$d)

#tapply: applies a function to subsets of a vector
head(warpbreaks)
with(warpbreaks, tapply(breaks, list(wool, tension), mean))

#by: applies a function to subsets of a data frame
by(iris[, 1:2], iris[, "Species"], summary)
by(iris[, 1:2], iris[, "Species"], sum)

#replicate: useful for generating data sets for simulation purposes. The final arguments turns
#the result into a vector or a matrix if possible
replicate(10, rnorm(10))
replicate(10, rnorm(10), simplify = TRUE)


#Split-Apply-Combine: II
#plyr package - slow on large data sets
#ddply: first d is data frame input, second d is data frame output
library(plyr)
result <- ddply(.data = subset(bnames2_b, year >= 2000),
                .variables = c('sex', 'name'),
                .fun = function(p){
                  summarize(p, tot = sum(tot))
                }
)


#OR
resultB <- ddply(subset(bnames2_b, year >= 2000), .(sex, name), summarize, tot = sum(tot)
                 )
#Run linear regression of RBIs in baseball dataset across a player's career in terms of years
head(baseball[, 1:16])
#Write a function that would take a data frame consisting of a subset for a specific player 
#and then return the regression model
rbi_vs_year <- function(df){
  df <- mutate(df, year = year - min(year))
  lm(rbi ~ year, data = df)
}
models <- dlply(baseball, .(id), rbi_vs_year)
models[[1]]
#Extract coefficients of each model object
coef(models[[1]])

#Now want data frame of coefficients, so start with a list and and want data frame
coefs <- ldply(models, coef)
ggplot(aes(x = `(Intercept)`, y = year), data = coefs) +
  geom_point()
qplot(`(Intercept)`, year, data = coefs)

#dplyr package - better with large data sets
Batting
#Want total number of games played by a player, arranged in decreasing order of totals
#In plyr:
games <- ddply(Batting, .(playerID), summarize, total = sum(G))
head(arrange(games, desc(total)))
#In dplyr:
library(dplyr)
games_d <- Batting %>%
  group_by(playerID) %>%
  summarize(total = sum(G)) %>%
  arrange(desc(total)) %>%
  head(5)

#data.table package:
library(data.table)
Batting_DT <- data.table(Batting)
games_dt <- Batting_DT[,
                       list(total = sum(G)),
                       "playerID"
                       ][
                         head(order(-total), 5),
                         ]
                       ]
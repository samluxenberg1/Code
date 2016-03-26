#USA Visualization

setwd("C:/Users/Samuel/Documents/Slide Rule/Data")
library(ggplot2)
expenditure <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/Visualization_Data.csv')
library(dplyr)
library(tidyr)
expenditure <- gather(expenditure, "year", "amount", 6:21)
exp_2012 <- subset(expenditure, expenditure$year == 'X2012')
df1 <- exp_2012[, 2]
df2 <- exp_2012[, 5]
df3 <- exp_2012[, 7]
exp_2012 <- data.frame(df1, df2, df3)

exp_2012_1 <- spread(exp_2012, df2, df3)
exp_num <- exp_2012_1[, 2:29]
apply(exp_num, 1, sum)
exp_2012_1$Recreation <- exp_2012_1$`Recreation services`+ exp_2012_1$`Recreational goods and vehicles`
exp_2012_1$Rec_Pct <- exp_2012_1$Recreation/apply(exp_num, 1, sum)

library(choroplethr)
library(choroplethrMaps)
choro_data <- data.frame(exp_2012_1[, 1], exp_2012_1[, 31])
names(choro_data)[1] <-"region"
names(choro_data)[2] <- "value"
choro_data <- subset(choro_data, region != "United States")
choro_data$region <- tolower(choro_data$region)
state_choropleth(choro_data, title = "Recreational Spending as Percentage of Personal Consumer Expenditure")

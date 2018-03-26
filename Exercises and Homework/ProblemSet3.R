#Udacity 2
#Diamonds Data Set
library(ggplot2)
data(diamonds)
summary(diamonds)

#Number of observations
length(diamonds[, 1])
#Number of variables
length(names(diamonds))

# Create a histogram of the price of
# all the diamonds in the diamond data set.
qplot(x = price, data = diamonds,
      color = I('black'), fill = I('#099DD9'))
#Long-tailed distribution with mean = $3,933 and median = $2,401
summary(diamonds$price)

#How many diamonds cost less than $500?
cheap500 <- NA
diamonds$cheap500 <- ifelse(diamonds$price < 500, 1, 0)
diamonds$cheap500 <- factor(diamonds$cheap500)
summary(diamonds$cheap500)
#How many diamonds cost less than $250?
cheap250 <- NA
diamonds$cheap250 <- ifelse(diamonds$price < 250, 1, 0)
diamonds$cheap250 <- factor(diamonds$cheap250)
summary(diamonds$cheap250)
#How many diamonds cost at least $15,000?
expensive <- NA
diamonds$expensive <- ifelse(diamonds$price >= 15000, 1, 0)
diamonds$expensive <- factor(diamonds$expensive)
summary(diamonds$expensive)

# Explore the largest peak in the
# price histogram you created earlier.

# Try limiting the x-axis, altering the bin width,
# and setting different breaks on the x-axis.
qplot(x = price, data = diamonds,binwidth = 20,
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(limits = c(0, 1500), breaks = seq(0, 1500, 50))
ggsave('priceHistogram.png')

# Break out the histogram of diamond prices by cut.

# You should have five histograms in separate
# panels on your resulting plot.

qplot(x = price, data = diamonds) +
  facet_wrap(~cut)

#What cut has the highest priced diamond?
by(diamonds$price, diamonds$cut, max)
#What cut has the lowest priced diamond?
by(diamonds$price, diamonds$cut, min)
#What cut has the lowest median?
by(diamonds$price, diamonds$cut, median)

qplot(x = price, data = diamonds) + 
  facet_wrap(~cut, scales = "free_y")

# Create a histogram of price per carat
# and facet it by cut. You can make adjustments
# to the code from the previous exercise to get
# started.

qplot(x = price/carat, data = diamonds) + 
  facet_wrap(~cut, scales = "free_y")

# Adjust the bin width and transform the scale
# of the x-axis using log10.
qplot(x = log10(price/carat), data = diamonds, binwidth = .05) + 
  facet_wrap(~cut, scales = "free_y") 
#OR
qplot(x = price/carat, data = diamonds, binwidth = .05) +
  facet_wrap(~cut, scales = "free_y") +
  scale_x_log10()

  
# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.
qplot(x = clarity, y = price, data = diamonds, geom = 'boxplot')
qplot(x = color, y = price, data = diamonds, geom = 'boxplot')
qplot(x = cut, y = price, data = diamonds, geom = 'boxplot')
by(diamonds$price, diamonds$clarity, summary)
by(diamonds$price, diamonds$color, summary)
by(diamonds$price, diamonds$cut, summary)

# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.
qplot(x = color, y = price/carat, data = diamonds, geom = 'boxplot')

#Investigat the weight of diamonds (carat) using a frequency polygon
#Use different binwidths to see how the freqency polygon changes.
#What carat size has a count greater than 2000?

qplot(x = carat, data = diamonds, geom = 'freqpoly')
qplot(x = carat, data = diamonds, geom = 'freqpoly', binwidth = 1)
qplot(x = carat, data = diamonds, geom = 'freqpoly', binwidth = .5)
qplot(x = carat, data = diamonds, geom = 'freqpoly', binwidth = .1)
qplot(x = carat, data = diamonds, geom = 'freqpoly', binwidth = .01) +
  scale_x_continuous(limits = c(0, 5.01), breaks = seq(0, 5, .1))

#Your task is to download a data set of your choice
# and create 2-5 plots that make use of the techniques from Lesson 3.

# You might use a simple histogram, a boxplot split over a categorical variable,
# or a frequency polygon. The choice is yours!

#Doing this exercise outside RStudio because dplyr and tidry are not available for the current version of R in Rstudio.
aid <- read.csv("~/Slide Rule/Data/aidperperson.csv", header = T)
aid1 <- gather(aid, "year", "amount", 2:49)
aid2 <- cbind(aid1[, 1], aid1[, 3:4])
by(aid2$amount, aid2$year, summary)
qplot(x = amount, data = aid2) +
  facet_wrap(~year, scales = "free_y")
ggsave('aidHistogram.png')
qplot(x = year, y = amount, data = aid2, geom = 'boxplot') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('aidBoxPlot.png')
qplot(x = amount, data = aid2, geom = 'freqpoly', binwidth = 10) +
  facet_wrap(~year, scales = "free_y")
ggsave('aidFreqPoly.png')

# Your task is to investigate the distribution of your friends'
# birth months and days.

# Here some questions you could answer, and we hope you think of others.

# **********************************************************************

bdays <- read.csv("C:/Users/Samuel/Documents/Slide Rule/Data/birthdaysExample.csv")
datesFormat <- as.Date(bdays$dates, "%m/%d/%Y")

#5 people share the same birthday as me (1/22)
table(datesFormat)

#Which month contains the most number of birthdays? March has 98 birthdays
#How many birthdays are in each month?
bdaymonth <- month(datesFormat)
bdayday <- day(datesFormat)
bdayyear <- year(datesFormat)
combine <- cbind(bdaymonth, bdayday, bdayyear)
df <- as.data.frame(combine)
qplot(x = bdaymonth, data = df, binwidth = 1,
      color = I('black'), fill = I('#099DD9')) +
  scale_x_discrete(limits = c(1, 12), breaks = seq(1, 12, 1))
monthcount <- NA
df$monthcount <- ifelse(df$bdaymonth > 0, 1, 0)
by(df$monthcount, df$bdaymonth, sum)

#Which day of the year has the most number of birthdays? The 14th has 48 birthdays
daycount <- NA
df$daycount <- ifelse(df$bdayday > 0, 1, 0)
by(df$daycount, df$bdayday, sum)

# Do you have at least 365 friends that have birthdays on everyday
# of the year?
sortdates <- df[order(bdaymonth, bdayday), ]
difference <- NA
sortdates$difference <- NA
for (i in 1:1032){
  sortdates$difference[i] <- sortdates[i+1, 2]-sortdates[i, 2]
}
summary(sortdates)
#Since the maximum of the difference variable is greater than 1, that means there is a period during the year
#which has at least 2 days in between birthdays. So I don't have at least 365 friends that have birthdays
#on everyday of the year.

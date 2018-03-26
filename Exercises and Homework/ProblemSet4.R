#Problem Set 4

# In this problem set, you'll continue
# to explore the diamonds data set.

# Your first task is to create a
# scatterplot of price vs x.
# using the ggplot syntax.
library(ggplot2)
data(diamonds)

ggplot(aes(x = x, y = price), data = diamonds) +
  geom_point()

#Correlations
cor.test(diamonds$price, diamonds$x, method = 'pearson')
cor.test(diamonds$price, diamonds$y, method = 'pearson')
cor.test(diamonds$price, diamonds$z, method = 'pearson')

# Create a simple scatter plot of price vs depth.
ggplot(aes(x = depth, y = price), data = diamonds) +
  geom_point()
# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units.
ggplot(aes(x = depth, y = price), data = diamonds) +
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(0, 80, 2))

#Correlation
cor.test(diamonds$depth, diamonds$price, method = 'pearson')

# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.
ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point() +
  xlim(0, quantile(diamonds$carat, probs = .99)) +
  ylim(0, quantile(diamonds$price, probs = .99))

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.

# Create a new variable for volume in the diamonds data frame.
# This will be useful in a later exercise.

# Don't make any adjustments to the plot just yet.
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x = volume, y = price), data = diamonds) +
  geom_point()
#How many have volume zero?
library(plyr)
count(diamonds$volume == 0)
#Note: You need to run this command in R to unload the plyr package. 
#The plyr package will conflict with the dplyr package in later exercises.
detach("package:plyr", unload=TRUE) 

#Correlation
cor.test(diamonds$price, diamonds$volume, method = 'pearson')
with(subset(diamonds, volume > 0 & volume < 800), cor.test(price, volume, method = 'pearson'))

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)
dsub <- subset(diamonds, volume < 800 & volume > 0)
ggplot(aes(x = volume, y = price), data = dsub) +
  geom_point(alpha = 1/50) +
  geom_smooth(method = 'lm', color = 'red')

# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

# where n is the number of diamonds in each
# level of clarity.
library(dplyr)
clarity_groups <- group_by(diamonds, clarity)
diamondsByClarity <- summarise(clarity_groups,
                               mean_price = mean(price),
                               median_price = median(as.numeric(price)),
                               min_price = min(price),
                               max_price = max(price),
                               n = n())
diamondsByClarity <- arrange(diamondsByClarity, clarity)

# We've created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))
# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.
library(gridExtra)
p1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +
  geom_bar(stat = 'identity')
p2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) +
  geom_bar(stat = 'identity')
grid.arrange(p1, p2, ncol = 2)

#Gapminder Data
# In your investigation, examine pairs of variable and create 2-5 plots that make
# use of the techniques from Lesson 4.
SAT <- read.csv("~/Slide Rule/Data/SAT_Results.csv", header = T)
N <- as.numeric(as.character(SAT$Number.of.Test.Takers))
CR_Mean <- as.numeric(as.character(SAT$Critical.Reading.Mean))
M_Mean <- as.numeric(as.character(SAT$Mathematics.Mean))
W_Mean <- as.numeric(as.character(SAT$Writing.Mean))
SAT_Num <- as.data.frame(cbind(N, CR_Mean, M_Mean, W_Mean))
SAT_Num <- subset(SAT_Num, N > 0)
SAT_Num$Total <- SAT_Num$CR_Mean + SAT_Num$M_Mean + SAT_Num$W_Mean
plot1 <- ggplot(aes(x = N, y = Total), data = SAT_Num) +
  geom_point() +
  xlim(0, quantile(SAT_Num$N, probs = .95)) +
  ylim(900, quantile(SAT_Num$Total, probs = .95)) +
  geom_smooth(method = 'lm', color = 'red')
#Moderate positive linear correlation between total SAT score and number of test takers
cor.test(SAT_Num$N, SAT_Num$Total, method = 'pearson')


plot2 <- ggplot(aes(x = CR_Mean, y = M_Mean), data = SAT_Num) +
  geom_point(color = 'orange') +
  geom_smooth(method = 'lm', color = 'black')

#Strong positive linear correlation between average math and average reading scores
cor.test(SAT_Num$CR_Mean, SAT_Num$M_Mean, method = 'pearson')

plot3 <- ggplot(aes(x = W_Mean, y = M_Mean), data = SAT_Num) +
  geom_point(color = 'blue') +
  geom_line(stat = 'summary', fun.y = mean)

grid.arrange(plot1, plot2, plot3, ncol = 1)
  

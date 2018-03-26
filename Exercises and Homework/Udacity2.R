#Udacity 2 (Actual)

setwd("C:/Users/Samuel/Documents/Slide Rule/Data")
library(ggplot2)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
qplot(x = age, y = friend_count, data = pf)
#Same as...
qplot(age, friend_count, data = pf)
#Using ggplot...
ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point()

summary(pf$age)

ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point(alpha = 1/20) + 
  xlim(13, 90)
#A little randomness never hurt anyone...
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_jitter(alpha = 1/20) + 
  xlim(13, 90)

ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point(alpha = 1/20) + 
  xlim(13, 90) +
  coord_trans(y = "sqrt")

#With Jitter, need to make sure only age jitters. Some users have zero friends to jitter could 
#add negative numbers and square roots of negative numbers are imaginary.
#Need special syntax
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point(alpha = 1/20, position = position_jitter(h = 0)) + 
  xlim(13, 90) +
  coord_trans(y = "sqrt")

# Examine the relationship between
# friendships_initiated (y) and age (x)
# using the ggplot syntax.

ggplot(aes(x = age, y = friendships_initiated), data = pf) +
  geom_point(alpha = 1/20, position = position_jitter(h = 0)) +
  xlim(13, 90)

#Solution
ggplot(aes(x = age, y = friendships_initiated), data = pf) +
  geom_jitter(alpha = 1/10)
#OR
ggplot(aes(x = age, y = friendships_initiated), data = pf) +
  geom_point(alpha = 1/10, position = 'jitter')
#If we want to transform data with square root, need to set height = 0
ggplot(aes(x = age, y = friendships_initiated), data = pf) +
  geom_point(alpha = 1/10, position = position_jitter(h = 0))

library(dplyr)
#group data frame by age
age_groups <- group_by(pf, age)
pf.fc_by_age <- summarise(age_groups, 
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n = n())
head(pf.fc_by_age)
 
#Alternative method to get same table
pf.fc_by_age <- pf %.%
  group_by(age) %.%
  summarise(friend_count_mean = mean(friend_count),
            friend_count_median = median(friend_count), 
            n = n()) %.%
  arrange(age)
head(pf.fc_by_age, 20)

# Plot mean friend count vs. age using a line graph.
# Be sure you use the correct variable names
# and the correct data frame. You should be working
# with the new data frame created from the dplyr
# functions. The data frame is called 'pf.fc_by_age'.

# Use geom_line() rather than geom_point to create
# the plot. 
ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age) +
  geom_line()

ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point(alpha = 1/20, position = position_jitter(h = 0),
             color = 'orange') + 
  xlim(13, 90) +
  coord_trans(y = "sqrt") +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, probs = .1,
            linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, probs = .9,
            linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, probs = .5,
            color = 'blue')
  
ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point(alpha = 1/20, position = position_jitter(h = 0),
             color = 'orange') + 
  coord_trans(y = "sqrt") +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, probs = .1,
            linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, probs = .9,
            linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = quantile, probs = .5,
            color = 'blue')+
  coord_cartesian(ylim = c(0,1000), xlim = c(13, 50))

#What is the (linear) correlation between age and friend counts?
cor.test(x = pf$age, y = pf$friend_count, method = "pearson")
#Alternative method for computing pearson correlation:
with(pf, cor.test(age, friend_count, method = 'pearson'))
with(subset(pf, age <= 70), cor.test(age, friend_count, method = 'pearson'))
with(subset(pf, age <= 70), cor.test(age, friend_count, method = 'spearman'))

# Create a scatterplot of likes_received (y)
# vs. www_likes_received (x).
ggplot(aes(x = www_likes_received, y = likes_received), data = pf) +
  geom_point(alpha = 1/20) +
  coord_cartesian(ylim = c(0, 5000), xlim = c(0, 5000))

#Get 95% of data
ggplot(aes(x = www_likes_received, y = likes_received), data = pf) +
  geom_point() +
  xlim(0, quantile(pf$www_likes_received, .95)) +
  ylim(0, quantile(pf$likes_received, .95)) +
  geom_smooth(method = 'lm', color = 'red')
#What's the correlation between the two variables? 
with(pf, cor.test(www_likes_received, likes_received, method = 'pearson'))

library(alr3)
data(Mitchell)
# Create a scatterplot of temperature (Temp)
# vs. months (Month).
names(Mitchell)
ggplot(aes(x = Month, y = Temp), data = Mitchell) +
  geom_point()
cor.test(Mitchell$Month, Mitchell$Temp, method = 'pearson')
#Break up x-axis in 12 month increments so it corresponds to a year. 
#What layer would you add to the plot?
range(Mitchell$Month)
ggplot(aes(x = Month, y = Temp), data = Mitchell) +
  geom_point() +
  scale_x_discrete(breaks = seq(0, 203, 12))

# Create a new variable, 'age_with_months', in the 'pf' data frame.
# Be sure to save the variable in the data frame rather than creating
# a separate, stand-alone variable. You will need to use the variables
# 'age' and 'dob_month' to create the variable 'age_with_months'.

# Assume the reference date for calculating age is December 31, 2013.
pf$age_with_months <- pf$age + (12-pf$dob_month)/12

# Create a new data frame called
# pf.fc_by_age_months that contains
# the mean friend count, the median friend
# count, and the number of users in each
# group of age_with_months. The rows of the
# data framed should be arranged in increasing
# order by the age_with_months variable.
age_with_months_groups <- group_by(pf, age_with_months)
pf.fc_by_age_with_months <- summarise(age_with_months_groups,
                                      friend_count_mean = mean(friend_count),
                                      friend_count_median = median(friend_count),
                                      n =())
pf.fc_by_age_with_months <- arrange(pf.fc_by_age_with_months, age_with_months)
  
#OR
pf.fc_by_age_with_months <- pf %.%
  group_by(age_with_months) %.%
  summarise(friend_count_mean = mean(friend_count),
            frient_count_median = median(friend_count),
            n = n()) %.%
  arrange(age_with_months)


head(pf.fc_by_age_with_months)

# Create a new scatterplot showing friend_count_mean
# versus the new variable, age_with_months. Be sure to use
# the correct data frame (the one you create in the last
# exercise) AND subset the data to investigate
# users with ages less than 71.

library(gridExtra)
ggplot(aes(x = age_with_months, y = friend_count_mean), data = subset(pf.fc_by_age_with_months, age_with_months < 71)) +
  geom_point()

p2 <- ggplot(aes(x = age_with_months, y = friend_count_mean), data = subset(pf.fc_by_age_with_months, age_with_months < 71)) +
  geom_line() +
  geom_smooth()
p1 <- ggplot(aes(x = age, y = friend_count_mean), data = subset(pf.fc_by_age, age < 71)) +
  geom_line() +
  geom_smooth()
p3 <- ggplot(aes(x = round(age/5)*5, y = friend_count), data = subset(pf, age < 71)) +
  geom_line(stat = 'summary', fun.y = mean)

grid.arrange(p3, p2, p1, ncol = 1)

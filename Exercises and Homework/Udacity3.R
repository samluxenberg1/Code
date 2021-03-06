#Udacity 3

setwd("C:/Users/Samuel/Documents/Slide Rule/Data")
library(ggplot2)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')

ggplot(aes(x = gender, y = age), data = subset(pf, !is.na(gender))) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = 'point', shape = 4)

ggplot(aes(x = age, y = friend_count), data = subset(pf, !is.na(gender))) +
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)

# Write code to create a new data frame,
# called 'pf.fc_by_age_gender', that contains
# information on each age AND gender group.

# The data frame should contain the following variables:

#    mean_friend_count,
#    median_friend_count,
#    n (the number of users in each age and gender grouping)
library(dplyr)
age_gender_groups <- group_by(subset(pf, !is.na(gender)), age, gender)
pf.fc_by_age_gender <- summarise(age_gender_groups,
                                 mean_friend_count = mean(friend_count),
                                 median_friend_count = median(as.numeric(friend_count)),
                                 n = n())
ungroup(pf.fc_by_age_gender)
arrange(pf.fc_by_age_gender, age)

#Solution
pf.fc_by_age_gender <- pf %.%
  filter(!is.na(gender)) %.%
  group_by(age, gender) %.%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(as.numeric(friend_count)),
            n = n()) %.%
  ungroup() %.%
  arrange(age)

# Create a line graph showing the
# median friend count over the ages
# for each gender. Be sure to use
# the data frame you just created,
# pf.fc_by_age_gender.
ggplot(aes(x = age, y = median_friend_count), data = pf.fc_by_age_gender) +
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)

library(reshape2)
#dcast for data frame, keep age, split gender since want male and female columns
pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender, age ~ gender, value.var = 'median_friend_count')

# Plot the ratio of the female to male median
# friend counts using the data frame
# pf.fc_by_age_gender.wide.

# Think about what geom you should use.
# Add a horizontal line to the plot with
# a y intercept of 1, which will be the
# base line. Look up the documentation
# for geom_hline to do that. Use the parameter
# linetype in geom_hline to make the
# line dashed.

# The linetype parameter can take the values 0-6:
# 0 = blank, 1 = solid, 2 = dashed
# 3 = dotted, 4 = dotdash, 5 = longdash
# 6 = twodash
ggplot(aes(x = age, y = female/male), data = pf.fc_by_age_gender.wide) +
  geom_line() +
  geom_hline(yintercept = 1, alpha = .3, linetype = 2)

# Create a variable called year_joined
# in the pf data frame using the variable
# tenure and 2014 as the reference year.

# The variable year joined should contain the year
# that a user joined facebook.
pf$year_joined <- floor(2014 - pf$tenure/365)
summary(pf$year_joined)
table(pf$year_joined)

# Create a new variable in the data frame
# called year_joined.bucket by using
# the cut function on the variable year_joined.

# You need to create the following buckets for the
# new variable, year_joined.bucket

#        (2004, 2009]
#        (2009, 2011]
#        (2011, 2012]
#        (2012, 2014]

# Note that a parenthesis means exclude the year and a
# bracket means include the year.
pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004, 2009, 2011, 2012, 2014))
table(pf$year_joined.bucket, useNA = 'ifany')

# Create a line graph of friend_count vs. age
# so that each year_joined.bucket is a line
# tracking the median user friend_count across
# age. This means you should have four different
# lines on your plot.

# You should subset the data to exclude the users
# whose year_joined.bucket is NA.
ggplot(aes(x = age, y = friend_count), data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median)

# Write code to do the following:

# (1) Add another geom_line to code below
# to plot the grand mean of the friend count vs age.

# (2) Exclude any users whose year_joined.bucket is NA.

# (3) Use a different line type for the grand mean.

# As a reminder, the parameter linetype can take the values 0-6:

# 0 = blank, 1 = solid, 2 = dashed
# 3 = dotted, 4 = dotdash, 5 = longdash
# 6 = twodash
ggplot(aes(x = age, y = friend_count), data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = mean, linetype = 2)

#How many friends does a user have for each day since using Facebook?
#Subset data to include only users with at least one day of tenure
#What's the median rate?
#What's the maximum rate?

pf$friend_rate <- pf$friend_count/pf$tenure
pf_rate <- subset(pf, tenure > 0)
summary(pf_rate$friend_rate)

#Solution
with(subset(pf, tenure >= 1), summary(friend_count/tenure))

# Create a line graph of mean of friendships_initiated per day (of tenure)
# vs. tenure colored by year_joined.bucket.

# You need to make use of the variables tenure,
# friendships_initiated, and year_joined.bucket.

# You also need to subset the data to only consider user with at least
# one day of tenure.
p1 <- ggplot(aes(x = tenure, y = friendships_initiated/tenure), data = subset(pf, tenure > 0)) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)
p2 <- ggplot(aes(x = 7 * round(tenure/7), y = friendships_initiated/tenure), data = subset(pf, tenure > 0)) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)
p3 <- ggplot(aes(x = 30 * round(tenure/30), y = friendships_initiated/tenure), data = subset(pf, tenure > 0)) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)
p4 <- ggplot(aes(x = 90 * round(tenure/90), y = friendships_initiated/tenure), data = subset(pf, tenure > 0)) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)

library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 1)

ggplot(aes(x = 7 * round(tenure/7), y = friendships_initiated/tenure), data = subset(pf, tenure > 0)) +
  geom_smooth(aes(color = year_joined.bucket))

#New Data Set
yo <- read.csv('yogurt.csv')
str(yo)
#Convert ID to a factor variable
yo$id <- factor(yo$id)
str(yo)
#Histogram of yogurt prices
ggplot(aes(x = price), data = yo) +
  geom_histogram()
#OR
qplot(x = price, data = yo, binwidth = .5)
#Non-graphical clues of discreteness
summary(yo)
#75th percentile and maximum are same number
length(unique(yo$price))
unique(yo$price)
#Clear discreteness from unique values
table(yo$price)
  
# Create a new variable called all.purchases,
# which gives the total counts of yogurt for
# each observation or household.

# One way to do this is using the transform
# function. You can look up the function transform
# and run the examples of code at the bottom of the
# documentation to figure out what it does.

# The transform function produces a data frame
# so if you use it then save the result to 'yo'!

# OR you can figure out another way to create the
# variable.
yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)
head(yo)
summary(yo$all.purchases)
qplot(x = all.purchases, data = yo, binwidth = 1)

# Create a scatterplot of price vs time.

# This will be an example of a time series plot.

# Resolve overplotting issues by using
# techniques you learned in Lesson 4.

# What are some things that you notice?
ggplot(aes(x = time, y = price), data = yo) +
  geom_point(alpha = 1/20)
#Solution
ggplot(aes(x = time, y = price), data = yo) +
  geom_jitter(alpha = 1/4, shape = 21, fill = I('#F79420'))

#Looking at samples of households
set.seed(4320)
sample.ids <- sample(levels(yo$id), 16)
#%in% loops over ids in order to create a panel plot
ggplot(aes(x = time, y = price), data = subset(yo, id %in% sample.ids)) +
  facet_wrap(~ id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)

#Use different seed
set.seed(32432)
sample.ids <- sample(levels(yo$id), 16)
ggplot(aes(x = time, y = price), data = subset(yo, id %in% sample.ids)) +
  facet_wrap(~id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)

#Scatter plot matrices
library(GGally)
theme_set(theme_minimal(20))

#Set seed for reproducible results
set.seed(1836)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
pf_subset <- pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000), ])

#Even more variables
nci <- read.table('nci.tsv')
#Change columns to produce nicer plot
colnames(nci) <- c(1:64)

#Melt data to long format
library(reshape2)
nci.long.samp <- melt(as.matrix(nci[1:200, ]))
names(nci.long.samp) <- c('gene', 'case', 'value')
head(nci.long.samp)

#Make heat map
ggplot(aes(y = gene, x = case, fill = value), data = nci.long.samp) +
  geom_tile() +
  scale_fill_gradientn(colours = colorRampPalette(c('blue', 'red'))(100))

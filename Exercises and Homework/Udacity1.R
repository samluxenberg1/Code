#Udacity 1

pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
names(pf)
library(ggplot2)
qplot(x = dob_day, data = pf) + 
  scale_x_discrete(breaks=1:31) + 
  facet_wrap(~dob_month, ncol = 3)
qplot(x=friend_count, data = subset(pf, !is.na(gender)), binwidth = 10) + 
  scale_x_continuous(limits = c(0,1000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(~gender)
table(pf$gender)
by(pf$friend_count, pf$gender, summary)
qplot(x = tenure, data = pf, binwidth = 30,
      color = I('black'), fill = I('#099DD9'))
qplot(x = tenure/365, data = pf, binwidth = .25,
      xlab = 'Number of years using Facebook',
      ylab = 'Number of users in sample',
      color= I('black'), fill = I('#099DD9')) + 
  scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7))

qplot(x = age, data = pf, binwidth = 1,
      xlab = 'Age of users', 
      ylab = 'Number of users in sample',
      color= I('black'), fill = I('#099DD9')) + 
  scale_x_continuous(breaks = seq(0, 113, 5), limits = c(0, 120))
library(gridExtra)
hist1 <- qplot(x = friend_count, data = subset(pf, !is.na(gender)), binwidth = 10,
               color = I('black'), fill = I('#099DD9')) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))
hist2 <- qplot(x = log10(friend_count + 1), data = subset(pf, !is.na(gender)), binwidth = .05,
               color = I('black'), fill = I('#099DD9')) + 
  scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, .25))
hist3 <- qplot(x = sqrt(friend_count), data = subset(pf, !is.na(gender)), binwidth = 1,
               color = I('black'), fill = I('#099DD9')) + 
  scale_x_continuous(limits = c(0, 35), breaks = seq(0, 35, 5))

grid.arrange(hist1, hist2, hist3)

#Frequency Plot
qplot(x = friend_count, data = subset(pf, !is.na(gender)), 
      binwidth = 10) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)

qplot(x = friend_count, y = ..count../sum(..count..),
      data = subset(pf, !is.na(gender)),
      xlab = 'Friend Count',
      ylab = 'Proportion of Users with that Friend Count',
      binwidth = 10, geom = 'freqpoly', color = gender) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

#Frequency Plot Exercise
qplot(x = www_likes, data = subset(pf, !is.na(gender)),
      binwidth = 1) + 
  scale_x_continuous(limits = c(0,115), breaks = seq(0, 115, 5)) + 
  facet_wrap(~gender)

qplot(x = www_likes, y = ..count../sum(..count..),
      data = subset(pf, !is.na(gender)),
      xlab = 'Internet Likes',
      ylab = 'Proportion of Users with that number of likes',
      binwidth = 1, geom = 'freqpoly', color = gender) + 
  scale_x_continuous(limits = c(0,115), breaks = seq(0, 115, 5))
  

#Solution
qplot(x = www_likes, data = subset(pf, !is.na(gender)),
      geom = 'freqpoly', color = gender) + 
  scale_x_continuous() + 
  scale_x_log10()
  
#Which gender has more www_likes?
table(pf$gender)
by(pf$www_likes, pf$gender, sum)

#Box Plot
#Adjusted friend count
pf_a <- subset(pf, friend_count < 1000)
qplot(x = gender, y = friend_count, 
      data = subset(pf_a, !is.na(gender)), 
      geom = 'boxplot')

#Solution
qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot',
      ylim = c(0, 1000))
#OR
qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') + 
  scale_y_continuous(limits = c(0, 1000))
#OR
qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') + 
  coord_cartesian(ylim = c(0, 250))
by(pf$friend_count, pf$gender, summary)

#Categorical Variable - Mobile Check-ins
summary(pf$mobile_likes)
summary(pf$mobile_likes > 0)
mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)

#What percentage of users have used mobile check-in?
sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in)

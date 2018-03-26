#Problem Set 5

setwd("C:/Users/Samuel/Documents/Slide Rule/Data")
library(ggplot2)

# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the histogram using
# scale_fill_brewer(type = 'qual')

data(diamonds)
ggplot(aes(x = price, fill = cut), data = diamonds) +
  geom_histogram() +
  facet_wrap(~color) +
  scale_fill_brewer(type = 'qual')

# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

# The plot should look something like this.
# http://i.imgur.com/rQF9jQr.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the scatterplot using
# scale_color_brewer(type = 'qual')
ggplot(aes(x = table, y = price), data = diamonds) +
  geom_point(aes(colour = cut)) +
  scale_color_brewer(type = 'qual')
by(diamonds$table, diamonds$cut, summary)

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

# Note: Volume is a very rough approximation of
# a diamond's actual volume.

# The plot should look something like this.
# http://i.imgur.com/excUpea.jpg

# Note: In the link, a color palette of type
# 'div' was used to color the scatterplot using
# scale_color_brewer(type = 'div')
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x = volume, y = price), data = diamonds) +
  geom_point(aes(colour = clarity)) +
  scale_y_log10() +
  xlim(0, quantile(diamonds$volume, probs = .99)) +
  scale_color_brewer(type = 'div')

# Many interesting variables are derived from two or more others.
# For example, we might wonder how much of a person's network on
# a service like Facebook the user actively initiated. Two users
# with the same degree (or number of friends) might be very
# different if one initiated most of those connections on the
# service, while the other initiated very few. So it could be
# useful to consider this proportion of existing friendships that
# the user initiated. This might be a good predictor of how active
# a user is compared with their peers, or other traits, such as
# personality (i.e., is this person an extrovert?).

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
pf$prop_initiated <- pf$friendships_initiated/pf$friend_count
pf$year_joined <- floor(2014 - pf$tenure/365)
pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004, 2009, 2011, 2012, 2014))

# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

# Recall, we created year_joined.bucket in Lesson 5
# by first creating year_joined from the variable tenure.
# Then, we used the cut function on year_joined to create
# four bins or cohorts of users.

# (2004, 2009]
# (2009, 2011]
# (2011, 2012]
# (2012, 2014]

# The plot should look something like this.
# http://i.imgur.com/vNjPtDh.jpg
# OR this
# http://i.imgur.com/IBN1ufQ.jpg
ggplot(aes(x = tenure, y = prop_initiated), data = subset(pf, !is.nan(prop_initiated))) +
  geom_line(aes(colour = year_joined.bucket), stat = 'summary', fun.y = median)

# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can use larger
# bins for tenure or add a smoother to the plot.
ggplot(aes(x = 90 * round(tenure/90), y = prop_initiated), data = subset(pf, !is.nan(prop_initiated))) +
  geom_line(aes(colour = year_joined.bucket), stat = 'summary', fun.y = median)

  ggplot(aes(x = tenure, y = prop_initiated), data = subset(pf, !is.nan(prop_initiated))) +
    geom_line(aes(colour = year_joined.bucket), stat = 'summary', fun.y = median) +
    geom_smooth()
  
  by(subset(pf, !is.nan(prop_initiated))$prop_initiated, 
     subset(pf, !is.nan(prop_initiated))$year_joined.bucket, mean)
  

  # Create a scatter plot of the price/carat ratio
  # of diamonds. The variable x should be
  # assigned to cut. The points should be colored
  # by diamond color, and the plot should be
  # faceted by clarity.
  
  # The plot should look something like this.
  # http://i.imgur.com/YzbWkHT.jpg.
  
  # Note: In the link, a color palette of type
  # 'div' was used to color the histogram using
  # scale_color_brewer(type = 'div')
  ggplot(aes(x = cut, y = price/carat), data = diamonds) +
    geom_jitter(aes(colour = color)) +
    facet_wrap(~ clarity) +
    scale_color_brewer(type = 'div')
  
  # In your investigation, examine 3 or more variables and create 2-5 plots that make
  # use of the techniques from Lesson 5.
  SAT <- read.csv("~/Slide Rule/Data/SAT_Results.csv", header = T)
  N <- as.numeric(as.character(SAT$Number.of.Test.Takers))
  CR_Mean <- as.numeric(as.character(SAT$Critical.Reading.Mean))
  M_Mean <- as.numeric(as.character(SAT$Mathematics.Mean))
  W_Mean <- as.numeric(as.character(SAT$Writing.Mean))
  SAT_Num <- as.data.frame(cbind(N, CR_Mean, M_Mean, W_Mean))
  SAT_Num <- subset(SAT_Num, N > 0)
  SAT_Num$Total <- SAT_Num$CR_Mean + SAT_Num$M_Mean + SAT_Num$W_Mean
  SAT_Num$CR_Mean.bucket <- cut(SAT_Num$CR_Mean, breaks = c(200, 400, 600, 800))
  SAT_Num$M_Mean.bucket <- cut(SAT_Num$M_Mean, breaks = c(200, 400, 600, 800))
  SAT_Num$W_Mean.bucket <- cut(SAT_Num$W_Mean, breaks = c(200, 400, 600, 800))
  SAT_Num$Total.bucket <- cut(SAT_Num$Total, breaks = c(800, 1200, 1600, 2000, 2400))
  
  table(SAT_Num$Total.bucket)
  ggplot(aes(x = N, y = Total), data = SAT_Num) +
    geom_point(aes(colour = M_Mean.bucket)) +
    scale_color_brewer(type = 'div')
  ggplot(aes(x = N, y = M_Mean), data = SAT_Num) +
    geom_line(aes(colour = CR_Mean.bucket), stat = 'summary', fun.y = mean) +
    geom_smooth(color = 'black')
  library(GGally)
  ggpairs(SAT_Num)
  
  

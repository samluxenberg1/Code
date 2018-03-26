#MIT Analytics Edge Week 3 - Popularity of Music Records

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
song <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/songs.csv')
str(song)
summary(song$year)

#How many observations in 2010?
str(subset(song, year == 2010))
#or
table(song$year)

#How many songs are by Michael Jackson
MichaelJackson <- subset(song, artistname == "Michael Jackson")
str(MichaelJackson)
#Which of his songs made it to the top 10?
which(MichaelJackson$songtitle == "Beat It") # = 13
MichaelJackson$Top10[13]
which(MichaelJackson$songtitle == "You Rock My World") # = 1
MichaelJackson$Top10[1]
which(MichaelJackson$songtitle == "Billie Jean") # = 5
MichaelJackson$Top10[5]
which(MichaelJackson$songtitle == "You Are Not Alone") # = 4
MichaelJackson$Top10[4]
#Or
MichaelJackson[c("songtitle", "Top10")]
table(song$timesignature)
which.max(song$tempo)
song[6206, ]

#Split data into training and testing
songTrain <- subset(song, year <= 2009)
songTest <- subset(song, year == 2010)

#Build Logistic Regression Model
#Use all numerical attributes
nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")
songTrain <- songTrain[, !(names(songTrain) %in% nonvars)]
songTest <- songTest[, !(names(songTest) %in% nonvars)]

Model1 = glm(Top10 ~ ., data = songTrain, family = binomial)
summary(Model1)
#For the confidence variables, all are significant and positive. So higher confidence leads to a higher
#predicted probability of a Top 10 hit.
#Low confidence is a sign of a more complex song. So, since high confidence is more likely to land the hit
#among the Top 10, this means that mainstream listeners tend to prefer less complex songs.

#Beware of Multicollinearity
cor(songTrain$loudness, songTrain$energy)

#Model2 (without loudness)
Model2 = glm(Top10 ~ . - loudness, data = songTrain, family = binomial)
summary(Model2)
#Here higher energy is associated with higher popularity in contradiction to Model 1. Here energy is also 
#insignificant.

#Model3 (without energy)
Model3 = glm(Top10 ~ . - energy, data = songTrain, family = binomial)
summary(Model3)

#Validating our model (3)
#Make predictions on test set using model 3. What is the accuracy of Model3 on the test set, using a 
#threshold of .45?
TestPrediction = predict(Model3, newdata = songTest, type = "response")
table(songTest$Top10, TestPrediction >= .45)
#Accuracy
(309+19) / (309+19+5+40) #= 88%

#Is Model 3 any better than baseline model?
#Easier model is to predict the most frequent outcome (a song is not a Top 10 hit) for all songs.
#What would the accuracy of the baseline model be on the test set?
(309+5)/(309+5+40+19) #= 84%
#or
table(songTest$Top10)

#Model 3 gives small improvement over baseline, but does it give it an edge?
#Think about this problem in terms of an investment.
#If a production company is interested in investing in songs that are highly likely to make it to
#Top 10, then the baseline model is useless, because it predicts no songs will be in Top 10. 

#How many songs does Model 3 correctly predict as Top 10 hits in 2010, using a threshold of .45? 19
#How many non-hit songs does it predict as Top 10? 5

#What is the sensitivity of the model 3, using threshold .45? (i.e. what is the probability of predicting
#a success correctly?)
19/(19+40) #= .322
#Specificity? (i.e. what is the probability of predicting a non-success correctly?)
309/(309+5) #= .984

#Conclusions:
#This model favors specificity over sensitivity. It also provides conservative predictions, and predicts
#that a song will make it to the Top 10 very rarely. SO while it detects less than half of the Top 10 songs, 
#we can be very confident in the songs that it does predict to be in Top 10. So it can offer a 
#competitive edge because of its conservative predictions. 

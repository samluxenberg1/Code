#MIT Analytics Edge Week 5 - Automated Reviews in Medicine

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
trials <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/clinical_trial.csv', stringsAsFactors = FALSE)
names(trials)
max(nchar(trials$abstract))
length(trials$abstract[nchar(trials$abstract) == 0])
#or
table(nchar(trials$abstract) == 0)
#or
sum(nchar(trials$abstract) == 0)

which.min(nchar(trials$title))
trials$title[1258]

#Because we have both title and abstract information  for trials, we have to build two corpera instead of one.
library(tm)
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))
corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))
corpuTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

sparseTitle <- removeSparseTerms(dtmTitle, .95)
sparseAbstract <- removeSparseTerms(dtmAbstract, .95)

dtmTitle <- as.data.frame(as.matrix(sparseTitle))
dtmAbstract <- as.data.frame(as.matrix(sparseAbstract))

str(dtmTitle)
str(dtmAbstract)

ncol(dtmTitle)
ncol(dtmAbstract)

#most frequent word stem across all abstracts
which.max(colSums(dtmAbstract))
colnames(dtmAbstract)[212]

#Combine into single data frame, but some of the variables have the same name 
colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))

dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial
ncol(dtm)

#Split
library(caTools)
set.seed(144)
split <- sample.split(dtm$trial, SplitRatio = .7)
train <- subset(dtm, split == TRUE)
test <- subset(dtm, split == FALSE)

table(train$trial)
#Accuracy of baseline model (always predicts non-trial) about 56.1%
table(test$trial)
(313)/(313+245)

library(rpart)
library(rpart.plot)
trialCART <- rpart(trial ~ ., data = train, method = "class")
prp(trialCART)

#Training set predictions for the model
pred.prob <- predict(trialCART)
#Extract the predicted probability of a result being a trial
trial.prob <- pred.prob[, 2]
max(trial.prob)

#How do you expect the maximum predicted probability to differ in the testing set?
#It will likely be exactly the same as in the training set
#Becasue the CART tree assigns the same predicted probability to each leaf node and there are a small number of leaf
#nodes compared to data points, we expect the same maximum predicted probability.

#Training set accuracy about 82.3%
trainpred <- predict(trialCART, type = "class")
table(train$trial, trainpred)
(631+441)/(631+99+131+441)
#or
table(train$trial, trial.prob >= .5)

#Accuracy on test set about 75.8%
testpred <- predict(trialCART,  newdata = test, type = "class")
table(test$trial, testpred)
(261+162)/(261+52+83+162)
testpred2 <- predict(trialCART, newdata = test)
table(test$trial, testpred2[, 2] >= .5)
#AUC about 83.7%
library(ROCR)
predROCR <- prediction(testpred2[, 2], test$trial)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
as.numeric(performance(predROCR, "auc")@y.values)

#Decision-Maker Tradeoffs

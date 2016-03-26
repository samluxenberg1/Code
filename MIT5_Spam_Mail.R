#MIT Analytics Edge Week 5 - Spam Mail

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
emails <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/emails.csv', stringsAsFactors = FALSE)
str(emails)
table(emails$spam)
max(nchar(emails$text))
which.min(nchar(emails$text))
emails$text[1992]

library(tm)
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus)
ncol(dtm)
nrow(dtm)
spdtm <- removeSparseTerms(dtm, .95)
ncol(spdtm)

emailsSparse <- as.data.frame(as.matrix(spdtm), row.names = FALSE)
colnames(emailsSparse) <- make.names(colnames(emailsSparse))
sort(colSums(emailsSparse))

emailsSparse$spam <- emails$spam

#How many word stems appear at least 5000 times in the ham (not spam) emails in the dataset?
sort(colSums(subset(emailsSparse, spam == 0)))
#How many word stems appear at least 1000 times in the spam emails?
sort(colSums(subset(emailsSparse, spam == 1)))

#List of most common words in the ham and spam emails are significantly different from each other. 

#Models
emailsSparse$spam <- as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
split <- sample.split(emailsSparse$spam, SplitRatio = .7)
train <- subset(emailsSparse, split == TRUE)
test <- subset(emailsSparse, split == FALSE)

#Logistic
spamLog = glm(spam ~ ., data = train, family = binomial)
predLog = predict(spamLog, type = "response")

table(predLog < .00001)
table(predLog > .99999)
table(predLog > .00001 & predLog < .99999)
summary(spamLog)
#None of the variables in the logistic regression are significant. This is a symptom of the algorithm not 
#converging due to being severely over-fit.
#Training set accuracy about 99.9%
table(train$spam, predLog >= .5)
(3052+954)/(3052+954+0+4)
#Taining AUC about 99.9996%
library(ROCR)
predLogROCR = prediction(predLog, train$spam)
as.numeric(performance(predLogROCR, "auc")@y.values)
#Testing Set accuracy about 95.05%
predTestLog = predict(spamLog, newdata = test, type = "response")
table(test$spam, predTestLog >= .5)
(1257+376)/(1257+51+34+376)
#Testing AUC about 96.28%
predLogTestROCR = prediction(predTestLog, test$spam)
as.numeric(performance(predLogTestROCR, "auc")@y.values)


#CART
library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~ ., data = train, method = "class")
predCART = predict(spamCART)
predCART.prob <- predCART[, 2]
prp(spamCART)
#Training set accuracy for spamCART about 94.2%
table(train$spam, predCART.prob >= .5)
(2885+894)/(2885+167+64+894)
#Training AUC about 96.96%
predTrainCARTROCR = prediction(predCART.prob, train$spam)
as.numeric(performance(predTrainCARTROCR, "auc")@y.values)
#Testing set accuracy about 93.95%
predCARTTest = predict(spamCART, newdata = test)
table(test$spam, predCARTTest[, 2] >= .5)
(1228+386)/(1228+80+24+386)
#Testing AUC about 96.32%
predTestCARTROCR = prediction(predCARTTest[, 2], test$spam)
as.numeric(performance(predTestCARTROCR, "auc")@y.values)

#Random Forest
library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data = train)
predRF = predict(spamRF, type = "prob")
predRF.prob <- predRF[, 2]
#Training set accuracy about 97.9%
table(train$spam, predRF.prob >= .5)
(3013+914)/(3013+39+44+914)
#Training AUC about 99.79%
predTrainRFROCR = prediction(predRF.prob, train$spam)
as.numeric(performance(predTrainRFROCR, "auc")@y.values)
#Testing set accuracy about 97.56%
predRFTest = predict(spamRF, newdata = test, type = "prob")
table(test$spam, predRFTest[, 2] >= .5)
(1290+386)/(1290+18+24+386)
#Testing AUC about 99.76%
predTestRFROCR = prediction(predRFTest[, 2], test$spam)
as.numeric(performance(predTestRFROCR, "auc")@y.values)


#Our random forest model outperformed both the logistic regression and CART models for the test set in terms of
#AUC and Accuracy

#Which model demonstrated greated degree of over-fitting?
#Both CART and random forest had very similar accuracies on the training and test sets. However, logistic regression
#obtained nearly perfect accuracy and AUC on the training set and had far from perfect performance on the test set.
#This is an indicator of over-fitting.


################################################################################################################
##Part 2

#Assigning weights to different types of errors
#Integrating Word count information

#word count for each email
wordCount <- rowSums((as.matrix(dtm)))
library(ggplot2)
#ggplot(aes(x = log(wordCount)), data = as.data.frame(wordCount)) +
 # geom_histogram()
  
hist(log(wordCount))
emailsSparse$logWordCount <- log(wordCount)
boxplot(emailsSparse$logWordCount ~ emailsSparse$spam)

train2 <- subset(emailsSparse, split == TRUE)
test2 <- subset(emailsSparse, split == FALSE)
spam2CART <- rpart(spam ~ ., data = train2, method = "class")
prp(spam2CART)
predspam2CART = predict(spam2CART, newdata = test2)
#Accuracy about 93.01%
table(test2$spam, predspam2CART[, 2] >= .5)
(1214+384)/(1214+94+26+384)
#AUC about 95.82%
predspam2CARTROCR = prediction(predspam2CART[, 2], test2$spam)
as.numeric(performance(predspam2CARTROCR, "auc")@y.values)

set.seed(123)
spam2RF <- randomForest(spam ~ ., data = train2)
#Accuracy about 97.73%
predspam2RF = predict(spam2RF, newdata = test2, type = "prob")
table(test2$spam, predspam2RF[, 2] >= .5)
(1296+383)/(1296+12+27+383)
#AUC about 99.81% 
predspam2RFROCR = prediction(predspam2RF[, 2], test2$spam)
as.numeric(performance(predspam2RFROCR, "auc")@y.values)

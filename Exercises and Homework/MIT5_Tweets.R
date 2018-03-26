#MIT Analytics Edge Week 5 - Tweets

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
tweets <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/tweets.csv', stringsAsFactors = FALSE)
#Need to add stringsAsFactors = FALSE when working with text analytics so that text is read in properly
str(tweets)
head(tweets)

#Interested in being able to detect the tweets with clear negative sentiment
tweets$Negative <- as.factor(tweets$Avg <= -1)
#This will set Negative == TRUE if the avg score is <= -1 and FALSE otherwise
table(tweets$Negative)

#Will be using bag of words approach to text mine
library(tm)
library(SnowballC)
#A corpus is a collection of documents. We'll need to convert our tweets to a corpus for pre-processing
#tm can create a corpus in many different ways, but we'll create one from the tweet column in our data set using
#two functions, Corpus and VectorSource.
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
#Check the documents match our tweets
corpus[[1]]
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)

stopwords("english")[1:10]
#remove the word 'apple' and all of the english stopwords
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
#Stem document
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content
#Bag of words to extract frequencies of words
#fucntion DocumentTermMatrix that generates a matrix where the rows correspond to documents, in our case tweets, 
#and the columns correspond to words in those tweets. The values in the matrix are the number of times that word
#appears in each document.  
frequencies = DocumentTermMatrix(corpus)
frequencies
#Look at documents 100 to 1005 and the words 505 to 515
inspect(frequencies[1000:1005, 505:515])
#Data is sparse i.e. lots of zeros in the matrix
#Look at the most popular words
findFreqTerms(frequencies, lowfreq = 20)
#There are only 56 words that appear at least 20 times in our tweets. This means that we probably have a lot of
#terms that will be pretty useless for our prediction model. 
#The number of terms is an issue for two reasons:
#1. Computational - more terms means more independent variables, which usually means it takes longer to build our 
#models
#2. In building models, the ratio of independent variables to observations  will affect how good the model will
#generalize.
#let's remove some terms that don't appear very often.
sparse = removeSparseTerms(frequencies, .995)
#.995 is the sparsity threshold. This means only keep terms that appear in .005 or .5% or more of the tweets.
sparse
#Convert sparse matrix into data frame that we can use for our predictive models
tweetsSparse = as.data.frame(as.matrix(sparse), row.names = FALSE)
#Since R struggles with variable names and we probably have some words here that start with numbers that start with 
#a number, if we run make.names function we'll make sure all our words are appropriate variable names.
#You should do this each time you build a data frame using text analytics.
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative

#Split the data
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = .7)
trainSparse = subset(tweetsSparse, split == TRUE)
testSparse = subset(tweetsSparse, split == FALSE)

#Data now ready to build predictive models
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data = trainSparse, method = "class")
prp(tweetCART)
#Our tree says that if the word "freak" is in the tweet then predict TRUE, or negative sentiment. If the word "freak"
#is not in the tweet, but the word "hate" is, again predict TRUE, negative sentiment. If the word "wtf" is in the 
#tweet the predict TRUE. If none of these three words is in the tweet then predict FALSE, or non-negative sentiment.

predictCART = predict(tweetCART, newdata = testSparse, type = "class")
table(testSparse$Negative, predictCART)
#Accuracy about 87.9%
(294+18)/(294+6+37+18)

#Baseline Model that always predicts non-negative sentiment
table(testSparse$Negative)
#Accuracy about 84.5%
(300)/(300+55)

#Random Forest Model
library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data = trainSparse)
predictRF = predict(tweetRF, testSparse)
table(testSparse$Negative, predictRF)
#Accuracy about 88.5%
(293+21)/(293+7+34+21)
#This is only a little better accuracy than our CART model, but due to the interpretability of our CART model, 
#CART model is preferable to random forest. If we were to use cross-validation for the CART model and use that cp
#paramter, the accuracy would improve to about the same as the random forest model. 

#So by using a bag of words approach and these models, we can reasonably predict sentiment even with a relatively
#small data set of tweets. 

#Logistic Regression
tweetLog = glm(Negative ~ ., data = trainSparse, family = binomial)
predictions = predict(tweetLog, newdata = testSparse, type = "response")
table(testSparse$Negative, predictions >= .5)
#Accuracy about 80.6%
(253+33)/(253+47+22+33)
#This accuracy is worse than the baseline model. If you were to compute the accuracy on the training set instead, 
#you would see that the model does really well on the training set - this is an example of over-fitting. The model
#fits the training set really well but does not perform well on the test set. A logistic regression with a large
#number of variables is particularly at risk for over-fitting.

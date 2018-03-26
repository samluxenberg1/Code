#MIT Analytics Edge Week 5 - Wikipedia Vandalism

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
wiki <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/wiki.csv', stringsAsFactors = FALSE)
head(wiki)
wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal)
library(tm)
corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)

dtmAdded <- DocumentTermMatrix(corpusAdded)
sparseAdded <- removeSparseTerms(dtmAdded, .997)
sparseAdded

wordsAdded <- as.data.frame(as.matrix(sparseAdded))

#preprend all words with the letter A
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))

#Repeat the above for removed words
corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)

dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, .997)

wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))

#prepend all words with the letter R
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))

str(wordsRemoved)

#Combine the two data frames into one
wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal

set.seed(123)
library(caTools)
split <- sample.split(wikiWords$Vandal, SplitRatio = .7)
train <- subset(wikiWords, split == TRUE)
test <- subset(wikiWords, split == FALSE)

#What is the accuracy of the baseline method which always predicts  non-vandalism on the test set?
table(test$Vandal)
(618)/(618+545)
#53.1%

#Build CART model to predict Vandal
library(rpart)
library(rpart.plot)
wikiTree <- rpart(Vandal ~ ., data = train, method = "class")
prp(wikiTree)

pred.wiki <- predict(wikiTree, newdata = test, type = "class")
table(test$Vandal, pred.wiki)
#Accuracy about 54.2%
(618+12)/(0+533+618+12)

#Although it beats the baseline, Bag of Words is not very predictive for this problem.  

pred.wiki.train <- predict(wikiTree, type = "class")
table(train$Vandal, pred.wiki.train)
(1443+33)/(1443+0+1237+33)
#Even on the training set the accuracy is only about 54.4% so it's definitly not over-fitting either.

#Other options
#Identifying a key class of words
#Counting words

#Key class of words
#The class of words we will use are website addresses.We hypothesize that given that a lot of vandalism seems to be
#adding links to promotional or irrelevant websites, the presence of a web address is a sign of vandalism.
#Search for this presence by searching for in the words added "http" in the Added column. The grepl function
#returns TRUE if a string is found in another string e.g. 
grepl("cat", "dogs and cats", fixed = TRUE) # TRUE
grepl("cat", "dogs and rats", fixed = TRUE) # FALSE

wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl("http", wiki$Added, fixed = TRUE), 1, 0)
table(wikiWords2$HTTP)

train2 <- subset(wikiWords2, split == TRUE)
test2 <- subset(wikiWords2, split == FALSE)
wikiTree2 <- rpart(Vandal ~ ., data = train2, method = "class")
prp(wikiTree2)
pred.wiki2 <- predict(wikiTree2, newdata = test2, type = "class")
table(test2$Vandal, pred.wiki2)
#Accuracy about 57.3%
(609+57)/(609+9+488+57)


#Another possibility is that the number of words added and removed is predictive, perhaps more so than the actual 
#words themselves. We already have a word count available in the form of the document-term matrices.
#Sum the rows of dtmAdded and dtmRemoved and add them as new variables in wikiWords2.
wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))
summary(wikiWords2$NumWordsAdded)

train3 <- subset(wikiWords2, split == TRUE)
test3 <- subset(wikiWords2, split == FALSE)
wikiTree3 <- rpart(Vandal ~ ., data = train3, method = "class")
prp(wikiTree3)
pred.wiki3 <- predict(wikiTree3, newdata = test3, type = "class")
table(test3$Vandal, pred.wiki3)
#Accuracy about 65.5%
(514+248)/(514+104+297+248)

#Using non-textual data
wikiWords3 <- wikiWords2
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin
train4 <- subset(wikiWords3, split == TRUE)
test4 <- subset(wikiWords3, split == FALSE)
wikiTree4 <- rpart(Vandal ~ ., data = train4, method = "class")
prp(wikiTree4)
pred.wiki4 <- predict(wikiTree4, newdata = test4, type = "class")
table(test4$Vandal, pred.wiki4)
#Accuracy about 71.9%
(595+241)/(595+23+304+241)

#By adding new independent variables we were able to make our model significantly more accurate without making it 
#more complicated.

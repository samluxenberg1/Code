#MIT Analytics Edge Week 5 - Enron

setwd('C:/Users/Samuel/Documents/Slide Rule/Data')
emails <- read.csv('C:/Users/Samuel/Documents/Slide Rule/Data/energy_bids.csv', stringsAsFactors = FALSE)
str(emails)
emails$email[1]
strwrap(emails$email[1])
#First emails deals with electricity markets, it doesn't have to do with energy schedules or bids. So it is not
#responsive to our query.
emails$responsive[1]
#nonresponsive
strwrap(emails$email[2])
emails$responsive[2]
#responsive
table(emails$responsive)
#the data set is unbalanced with a relatively small proportion of emails responsive to the query. This is typical
#in predictive coding problems.

#Pre-processing
library(tm)
corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]])

#Build Document Term Matrix for our corpus
dtm = DocumentTermMatrix(corpus)
dtm
#While we have only 855 emails, we have over 22,000 terms that showed up at least once, which is clearly too many
#variables.
#Remove the terms that don't appear too often.
dtm = removeSparseTerms(dtm, .97)
dtm

labeledTerms = as.data.frame(as.matrix(dtm), row.names = FALSE)
#So this data frame is only including the frequencies of the words that appear in at least 3% of the documents, but 
#in order to run our text analytics models, we're also going to have the outcome variables, which is whether or not
#each email was responsive.

labeledTerms$responsive = emails$responsive
str(labeledTerms)

#Split data
library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, SplitRatio = .7)
train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

#CART Model
library(rpart)
library(rpart.plot)
emailCART = rpart(responsive ~ ., data = train, method = "class")
prp(emailCART)
#If the word california appears at least twice, we're going to predict the document is responsive
pred = predict(emailCART, newdata = test)
pred[1:10, ]
#The left column is the predicted probability of the document being non-responsive and the right column is the
#predicted probabilty that the document is responsive.
pred.prob = pred[, 2]
table(test$responsive, pred.prob >= .5)
#Accuracy about 85.6%
(195+25)/(195+20+17+25)

#Baseline Model
#Always predicts non-responsive
table(test$responsive)
#Accuracy about 83.7%
215/(215+42)
#So there is a small improvement in accuracy using the CART model, which is a common case in unbalanced datasets.
#However in many document retrieval applications, there are uneven costs for different types of errors. 
#Since there would still be more manual work checking if a document is actually responsive, false positives, or 
#labeling a document as responsive when it is not creates additional work in the manual review process but no
#further harm because the manual review process will remove this erroneous result.
#On the other hand if there is a false negative in which a responsive document is labeled as non-responsive by our
#model, we will miss the doument entirely. Therefore, we're going to assign higher cost to false negatives than to 
#false positives. 
#Let's experiment with other cutoffs on our ROC curve.
library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE)
#Best cutoff to select depends on the costs assigned by the decision maker to false postives and true positives.
#However we do favor cutoffs that give us a high sensitivity i.e. high values of TP/(TP+FN).
#Here about 70% for true positive rate looks good, meaning that we're getting about 70% of all responsive documents
#and a false positive rate of about 20%, meaning that we're making mistakes and accidentally identifying as
#20% of the non-responsive documents.
#Since the vast majority of the documents are non-responsive, this cutoff would result in perhaps a large decrease 
#in manual effort needed in the eDiscovery process. 
#Looking at the blue color, we're looking at a threshold of around.15, significantly lower than 50%, which is 
#definitely what we would expect since we favor false positives over false negatives. 

#AUC about 79.4%
performance(predROCR, "auc")@y.values
#So our model can differentiate between a randomly selected responsive and non-responsive document almost 80% of the
#time.
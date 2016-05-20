library(data.table)
library(stringr)

library(tm)
library(qdap)
#library(openNLP)

numRow <-5000
twitter<-scan("final/en_US/en_US.twitter.txt", nmax=numRow, what=character(), sep = "\n")
news<-scan("final/en_US/en_US.news.txt", nmax=numRow, what=character(), sep = "\n")
#set.seed(10)
#TrainData<-sample(d, numTrainRow)

trainText<-sent_detect_nlp(c(news,twitter))

trainCorpus <- VCorpus(VectorSource(trainText))
trainCorpus <- tm_map(trainCorpus, removePunctuation)
trainCorpus <- tm_map(trainCorpus, stripWhitespace)

dtm <- DocumentTermMatrix(trainCorpus)
dtm2 <- as.matrix(dtm)
freq<-sort(colSums(dtm2), decreasing =T)

VCorpus(x, readerControl = list(reader = reader(x), language = "en"))


library(rsconnect)
rsconnect::setAccountInfo(name='lotgon', token='7CF91EB4A8AC9CE2A22C3740B502342D', secret='9oPBVU2zlMuVSd4tHS6/h1W55kOLGgnCbBgGHrfu')
rsconnect::setAccountInfo(name='lotgon', token='6C4E822A449B09BDD359BBD5EC953267', secret='tazMO13gFsJutKcYxI5lqLqASOiu8T5v+ain0wf+')

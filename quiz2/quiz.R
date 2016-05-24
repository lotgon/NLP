library(tm)
source("Common.R")
data<- scan("quiz3\\questions.txt", what=character(), sep="\n")
corpus<-VCorpus(VectorSource(data))
corpus<- PreProcessCorpus(corpus)
result<-list()
resultKneser<-list()
for(i in seq_along(corpus))
{
  tokens<-WordTokenizer(corpus[[i]])
  n<-length(tokens)
  last3 <- trimws(paste(tokens[n-2], tokens[n-1], tokens[n]))
  print(last3)
  result[[i]]<-Naive(last3)
  print(result[[i]])
  
  resultKneser[[i]]<-HighestPkn(last3)
  print(resultKneser[[i]])
}

#result[[1]][grepl("give|die|eat|sleep", result[[1]]$token)]
#result[[2]][grepl("horticultural|spiritual|marital|financial", result[[2]]$token)]
result[[5]][grepl("walk|picture|minute|look", result[[5]]$token)]
result[[6]][grepl("account|case|incident|matter", result[[6]]$token)]
resultKneser[[6]][grepl("account|case|incident|matter", resultKneser[[6]]$token)]
#result[[7]][grepl("hand|finger|arm|toe", result[[7]]$token)]
#result[[9]][grepl("inside|weekly|outside|daily", result[[9]]$token)]

PreProcessLastKWordsinCorpus<-function(grams, k)
{
  corpus<-VCorpus(VectorSource(grams))
  corpus<- PreProcessCorpus(corpus)
  tokens<-WordTokenizer(corpus[[1]])
  n<-length(tokens)
  paste(tokens[ seq.int(max(n-k+1, 0), n)] , collapse = " ")
}

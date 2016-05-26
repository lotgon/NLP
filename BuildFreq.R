library(stringr)
library(stringi)
library(tm)
library(qdap)
library(ggplot2)
library(RWeka)
library(data.table)
source("kneter.R")
source("Common.R")
inputDir<-"train"

if( !file.exists(paste0("gram", 1, ".txt") ) )
{
  for(j in 1:4)
  {
    jgram <- data.table()
    
    for(i in 0:78)
    {
      rawTrainCorpus<-VCorpus(DirSource(file.path(inputDir, "en_US"), encoding = "UTF-8", pattern=paste0("txt", i, "$")), readerControl=list(language="english"))
      trainCorpus<- PreProcessCorpus(rawTrainCorpus)
      rm(rawTrainCorpus)
      sapply(trainCorpus, object.size)
  
      tdm <- TermDocumentMatrix(trainCorpus, control = list(wordLengths=c(0, Inf), stopwords=F, tokenize = function(x){NGramTokenizer(x, Weka_control(min = j, max = j))}))
      freq <- rowSums(as.matrix(tdm))
    
      jgram<-rbindlist( list(jgram, as.data.table(data.frame(token=names(freq), N=freq, stringsAsFactors=F)) ))
    }
    setkey(jgram, "token")
    jgram<-jgram[,.(N=sum(N)), by=token]
    write.table(jgram, paste0("gram", j, ".txt"), row.names = F)
  }
}

ngramFreq<-list()
for(j in 1:4)
{
  ngramFreq[[j]]<-fread(paste0("gram", j, ".txt"))
  setkey(ngramFreq[[j]], token)
} 
#filter
for(j in 1:4)
  ngramFreq[[j]]<-ngramFreq[[j]][N>1]

sapply( 2:4, BuildN1Tables)

for(j in 1:4)
  write.table(ngramFreq[[j]], paste0(j, "Cn1gram.txt"), row.names = F)
for(j in 1:4)
  zip(paste0(j, "Cn1gram.zip"), paste0(j, "Cn1gram.txt"))



  

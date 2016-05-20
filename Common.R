SampleCopyFilesInDirectory <- function (inputDir, outputDir, sampleSize=1000, isOverwrite=F)
{
  if( dir.exists(outputDir))
  {
    if(isOverwrite)   
      unlink(outputDir, recursive = T)
    else
      return()
  }

  dir.create(outputDir)
  
  sapply(dir(inputDir), function (subdir)
  {
    if( dir.exists(file.path(inputDir, subdir)) )
      SampleCopyFilesInDirectory(file.path(inputDir, subdir), file.path(outputDir, subdir), sampleSize, isOverwrite)
    else
      CopyFileSplitted(file.path(inputDir, subdir), file.path(outputDir, subdir), sampleSize)
  }) 
}
SampleCopyFile <- function (sourceFile, outputFile, sampleSize)
{
  data<- scan(sourceFile, what=character(), sep = "\n", encoding="UTF-8")
  if( sampleSize > length(data))
    sampleSize = length(data)
  inTrain<-rbinom(sampleSize, 1, 0.4)
  write(data[inTrain], outputFile, sep="\n")  
}
CopyFileSplitted <- function (sourceFile, outputFile, sampleSize)
{
  i=0
  while(T)
  {
    start = i * sampleSize
    data<- scan(sourceFile, what=character(), sep = "\n", encoding="UTF-8", skip=start, nlines=sampleSize)
    write(sample(data, round(length(data)*0.4)), paste0(outputFile, i), sep="\n")  
    if( length(data) < sampleSize)
      break()
    i=i+1
  }
}

PreProcessCorpus <- function ( corpus )
{
  transFuncs<- list(removePunctuation, stripWhitespace, removeNumbers, content_transformer( function(x) iconv(enc2utf8(x), sub = " ")), 
                    content_transformer(tolower))
  corpus <- tm_map(corpus, tm_reduce, tmFuns = transFuncs)
  #tm_map(corpus, removeWords, stopwords())
}

#trainCorpus.dtm <- DocumentTermMatrix(trainCorpus)
#m <- as.matrix(trainCorpus.dtm)
#freq<-sort(colSums(m), decreasing = T)
#ggplot(data.frame( word=names(freq[1:100]), freq=freq[1:100]), aes(x=word, y=freq)) + geom_bar(stat='identity', fill="grey")
#plot(hc <- hclust(dist(dtm, method="binary")), main="Jaccard Dist")
#trainCorpus[[1]]$content[[1]]

#microbenchmark(
#  sum(stri_startswith_fixed(one_gram$token, "zyrtec"))  ,
  #one_gram[stri_startswith_fixed(one_gram$token, "zyrtec"),.N],
  #sum(grepl("^zyrtec", one_gram$token))
#)

#microbenchmark(
  #one_gram[token=="zyrtec", N],
  ###st=one_gram[fmatch("zyrtec", token), N]
#)

#test<-VCorpus(VectorSource("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"))
#test<-PreProcessCorpus(test)
##PBO_n("bacon bouquet case")

#library(parallel)
#library(doSNOW)
#cl<-makeCluster(4)

#registerDoSNOW(cl)
#stopcluster(cl)

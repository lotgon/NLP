
PBO_n <-function(n, gram)
{
  count_gramNMinus1 <- ngramFreq[[n-1]][gram==token, N]
  count_gramNMinus1 <- gt(n-1, count_gramNMinus1)
  if( length(count_gramNMinus1)==0)
    return (0)
  ngramFreq[[n]][stri_startswith_fixed(token, paste0(gram, " "))][,.(token, PBO_n = gt(n, N) / count_gramNMinus1)]
}

Naive <- function(gram)
{
  corpus<-PreProcessCorpus(VCorpus(VectorSource(gram)))
  tokens<-NGramTokenizer(corpus[[1]], Weka_control(min = 1, max = 1))
  
  result<-data.table()
  reducedCoef<-1
  
  while(T)
  {
    n<-length(tokens)
    if( n<1 )
      break()
    r<-PBO_n( n+1, paste( tokens, collapse = " ") )
    if( r!=0)
    {
      r[,PBO_n:=PBO_n*reducedCoef]    
      result<-rbindlist(list(result, r))
    }
    
    reducedCoef<-reducedCoef*0.4
    tokens<-tokens[-1]
  }
  result[order(-PBO_n)]
}




gt<-function (j, count)
{
  gtList[[j]] [data.table(c=count)]$ca
}


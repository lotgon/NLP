
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
  if( length(gram) == 0 )
    return (NA)
  
  isCompleteTokens<-F
  if( stri_sub(gram, -1, -1) ==' ')
    isCompleteTokens<-T
  
  tokens<-PreProcessLastKWordsinCorpus(gram, 3 + 1-isCompleteTokens)
  searchString<-paste( tokens, collapse = " ")
  
  if( !isCompleteTokens )  
    tokens<-tokens[-length(tokens)]
  else
    searchString<-paste0( searchString, " ")
  
#  
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
  result[stri_startswith_fixed(token, searchString)][order(-PBO_n)]
}




gt<-function (j, count)
{
  gtList[[j]] [data.table(c=count)]$ca
}


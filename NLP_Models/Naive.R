
PBO_n <-function(ngramFreq, gtList, n, gram)
{
  count_gramNMinus1 <- ngramFreq[[n-1]][gram==token, N]
  if( length(count_gramNMinus1)==0)
    return (data.frame())
  count_gramNMinus1 <- gt(gtList, n-1, count_gramNMinus1)
  if( length(count_gramNMinus1)==0)
    return (data.frame())
  ngramFreq[[n]][stri_startswith_fixed(token, paste0(gram, " "))][,.(token, PBO_n = gt(gtList, n, N) / count_gramNMinus1)]
}

Naive <- function(ngramFreq, gtList, gram)
{
  if( nchar(gram) == 0 )
    return (data.table())
  
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
    r<-PBO_n(ngramFreq, gtList, n+1, paste( tokens, collapse = " ") )
    if( nrow(r)!=0)
    {
      r[,PBO_n:=PBO_n*reducedCoef]    
      result<-rbindlist(list(result, r))
    }
    
    reducedCoef<-reducedCoef*0.4
    tokens<-tokens[-1]
  }
  if(!isCompleteTokens)
    result<-result[stri_startswith_fixed(token, searchString)]
  result[order(-PBO_n)]
}




gt<-function (gtList, j, count)
{
  gtList[[j]] [data.table(c=count)]$ca
}



HighestPkn<- function (gram)
{
  if( length(gram) == 0 )
    return (NA)
  
  isCompleteTokens<-F
  if( stri_sub(gram, -1, -1) ==' ')
    isCompleteTokens<-T
  
  inputTokens<-PreProcessLastKWordsinCorpus(gram, 3 + 1-isCompleteTokens)
  searchString<-paste( inputTokens, collapse = " ")
  
  if( !isCompleteTokens )  
    inputTokens<-inputTokens[-length(inputTokens)]
  else
    searchString<-paste0( searchString, " ")
    
  
  collapseTokens<-paste( inputTokens, collapse = " ")
  
  n <- length(inputTokens) + 1
  if( n==1 )
    return (data.table(token="I need more information"))
  
  count_gramNMinus1 <- ngramFreq[[n-1]][collapseTokens==token, N]
  if( length(count_gramNMinus1) == 0 )
    return (HighestPkn(paste( inputTokens[-1], collapse = " ")))
  
  N1_gramNMinus1<- ngramFreq[[n-1]][collapseTokens, N1_Start]
  
  ngramFreq[[n]][stri_startswith_fixed(token, searchString)][,.(tokenWithoutFirst=substr(token, nchar(collapseTokens)+2, nchar(token)), N, token)][
               , .(token, probability=(N-0.5)/count_gramNMinus1 + 0.5/count_gramNMinus1 * N1_gramNMinus1 * Pkn(tokenWithoutFirst, inputTokens[-1]))][
                 order(-probability)]
}
Pkn <- function (wi_tokens, preTokens)
{
  if(length(wi_tokens) < 1 )
    return (NA)
  n=length(preTokens) + 1
  if( n== 1)
    return( ngramFreq[[1]] [wi_tokens,N1_End/nrow(ngramFreq[[2]])] )
  
  collapsePreTokens<-paste( preTokens, collapse = " ")
  collapseAllTokens<-sapply(wi_tokens, function(x) paste(collapsePreTokens, x))
  
  first <- ngramFreq[[n]] [collapseAllTokens, N1_End - 0.5] / ngramFreq[[n-1]][collapsePreTokens, N1_Mid]
  first[first<0]<-0
  
  second<- 0.5 / ngramFreq[[n-1]][collapsePreTokens, N1_Mid] * ngramFreq[[n-1]][collapsePreTokens, N1_Start] * Pkn(wi_tokens, preTokens[-1])
  
  first + second
}

BuildN1Tables <- function (n)
{
  if( n>1)
  {
    temp<-ngramFreq[[n]][,.(token=gsub("(.*) [^ ]*$", "\\1", token))] [,.(N1_Start=.N), by=token]
    setkey(temp, "token")
    ngramFreq[[n-1]]<<-merge(temp, ngramFreq[[n-1]], all=T)
    
    temp<-ngramFreq[[n]][,.(token=gsub("^.*? ", "", token))] [,.(N1_End=.N), by=token]
    setkey(temp, "token")
    ngramFreq[[n-1]]<<-merge(temp, ngramFreq[[n-1]], all=T)
    
  }
  if( n > 2 )
  {
    temp<-ngramFreq[[n]][,.(token=sub("^.*? (.*) .*?$", "\\1", token))] [,.(N1_Mid=.N), by=token]
    setkey(temp, "token")
    ngramFreq[[n-2]]<<-merge(temp, ngramFreq[[n-2]], all=T)
  }
}


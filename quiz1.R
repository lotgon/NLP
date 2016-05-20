twitter<-scan("final/en_US/en_US.twitter.txt", what=character(), sep = "\n")
news<-scan("final/en_US/en_US.news.txt", what=character(), sep = "\n")
blogs<-scan("train/en_US/en_US.blogs.txt", what=character(), sep = "\n")

#twitterRu<-scan("final/ru_RU/ru_RU.twitter.txt", what=character(), sep = "\n")
#sum(grepl("любл",twitterRu)) / sum(gr epl("ненави",twitterRu))
#2
length(twitter)

#3
max(sapply(news, nchar))
max(sapply(twitter, nchar))
max(sapply(blogs, nchar))

#4
#In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
library(tm)
getTokenizers()

#tokens<-MC_tokenizer(twitter)
#sum(tokens == "love")/sum(tokens == "hate")
sum(grepl("love",twitter)) / sum(grepl("hate",twitter))

#5
#The one tweet in the en_US twitter data set that matches the word "biostats" says what?

grep("biostats", twitter, value=T)

#5
#How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (I.e. the line matches those characters exactly.)
grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitter, value=T)

##Group:
##
##name:
##
##Id:
text_bible <- scan("1581-0.txt",what="character",skip=156)
n <- length(text_bible)
text_bible <- text_bible[-((n-2909):n)]; ## strip license

##split the punction from words
splitPunct <- function(text_0){
  
  #mark the punction by " "
  text_punctmark <- gsub('([[:punct:]])', ' \\1 ', text_0)
  
  #split the text by " "
  text_split <- unlist(strsplit(text_punctmark," "))  
  
  return(text_split)
}

text_bible <- splitPunct(text_bible)

library(mgcv)
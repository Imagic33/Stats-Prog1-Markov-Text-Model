##Group:
##
##name:
##
##Id:
text_bible <- scan("1581-0.txt", what="character", skip=156, encoding = "UTF-8")
n <- length(text_bible)
text_bible <- text_bible[-((n-2909):n)]; ## strip license

##split the punction from words
splitPunct <- function(text_0){
  
  #Give the position for the punctuation, mark the punction by " "
  text_punctmark <- gsub('([[:punct:]])', ' \\1 ', text_0)
  
  #split the text by " "
  text_split <- unlist(strsplit(text_punctmark, " "))  
  
  return(text_split)
}

text_bible <- splitPunct(text_bible)

library(mgcv)
#Change the uppercase letter to lowercase
text_bible <- tolower(text_bible)

#find the unique words
text_unique <- unique(text_bible)

word_index <- match(text_bible, text_unique)

#Find the frequencies of each words
words_quantity <- tabulate(word_index)

#Find the most frequent 1000 words
rank_1001 <- sort(words_quantity, decreasing = TRUE)[1001]
word_1k <- text_unique[which(words_quantity > rank_1001)]
  
word_index_1k <- match(text_bible, word_1k)

word_matrix <- cbind(word_index_1k[-length(word_match)], word_index_1k[-1])
#get the common words pair
word_matrix <- word_matrix[-which(is.na(rowSums(word_matrix))),]

word_length <- length(word_1k)
A <- matrix(0, nrow = word_length, ncol = word_length)

nrow(word_matrix)
for (i in 1:nrow(word_matrix)) {
  a <- word_matrix[i,1]
  b <- word_matrix[i,2]
  A[a, b] = A[a, b] + 1
}

A <- A / rowSums(A)
Ran_sentence <- array("",c(1,100))
Ran_sentence[1] <- sample(b,1)
for (i in 1:100) {
  
  Ran_sentence[i+1] <- sample(b,1,T,A[which(b==Ran_sentence[i]),])
  
}
cat(Ran_sentence)



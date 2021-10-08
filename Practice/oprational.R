## Group
##
## name:
##
## Id:
setwd("C:/Users/Éò¶÷ÁÙ/Stats-Prog1-Markov-Text-Model/Work")
a <- scan("1581-0.txt", what="character", skip=156, encoding = "UTF-8")
n <- length(a)
a <- a[-((n-2909):n)] ## strip license
# a is the bible words




split_punct <- function(words){
  
    pri_punct_pos <- grep("[[:punct:]]", a, perl=TRUE) 
    ## Find the words with punctuation.
    bible_ws <- rep("", length(a) + length(pri_punct_pos))
    ## Create an empty character list to be filled.
    punct_pos <- pri_punct_pos + 1:length(pri_punct_pos)
    ## Give the position for the punctuation.
    bible_ws[punct_pos] <- gsub("[[:alnum:]]", "", a[pri_punct_pos])
    ## Eliminate the other character to leave punctuation alone.
    bible_ws[-punct_pos] <- gsub("[[:punct:]]", "", a)
    ## Eliminate the punctuation.
    return(bible_ws)
    
}
bible_ws <- split_punct(a)

##There is a character that influence tolower.
bible_ws_low <- tolower(bible_ws)
# Transform the words in bible into lower case.
bible_voc <- unique(bible_ws_low)
# Find all the vocabulary in bible.
word_pos <- match(bible_ws_low,bible_voc)
# Find the positions for each words.
freq <- tabulate(word_pos)
## Find the frequencies of each words.
main_voc_pos <- order(freq,decreasing=TRUE)[1:1000]
## Find the most frequent 1000 words.
main_voc <- bible_voc[main_voc_pos]

b <- main_voc

b_position <- match(bible_ws_low,b)
# Find the positions for each words for vector b.

bible_ws_low[b_position]
follow_pri <- b_position[-1]
follow <- append(follow_pri, b_position[1])

pair_pos <- cbind(b_position, follow)
cbind(b_position, follow)

##delete_pair_pos  

pair_pos <- pair_pos[-which(is.na(rowSums(pair_pos))),]
pair_pos
A <- matrix(0,1000,1000) 

for (i in 1:nrow(pair_pos )) {
  
  fir_pos <- pair_pos [i,1]
  sec_pos <- pair_pos [i,2] 
  A[fir_pos,sec_pos] <- A[fir_pos,sec_pos] + 1
  
  
}

A <- A/rowSums(A)
Ran_sentence <- array("",c(1,100))
Ran_sentence[1] <- sample(b,1)
for (i in 1:100) {
  
  Ran_sentence[i+1] <- sample(b,1,T,A[which(b==Ran_sentence[i]),])
  
}
cat(Ran_sentence)

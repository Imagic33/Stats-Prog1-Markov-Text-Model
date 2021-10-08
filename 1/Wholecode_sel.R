## Group
##
## name:
##
## Id:
setwd("C:/Users/Éò¶÷ÁÙ/Stats-Prog1-Markov-Text-Model/Work")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license
# a is the bible words




split_punct <- function(words){
  
  pri_punct_pos <- grep("[[:punct:]]",a,perl=TRUE) 
  ## Find the words with punctuation.
  bible_ws <- rep("",length(a)+length(pri_punct_pos))
  ## Create an empty character list to be filled.
  punct_pos <- pri_punct_pos + 1:length(pri_punct_pos)
  ## Give the position for the punctuation.
  bible_ws[punct_pos] <- gsub("[[:alnum:]]","",a[pri_punct_pos])
  ## Eliminate the other character to leave punctuation alone.
  bible_ws[-punct_pos] <- gsub("[[:punct:]]","",a)
  ## Eliminate the punctuation.
  return(bible_ws)
  
}
bible_ws <- split_punct(a)
bible_words <- gsub("â€","",bible_ws)
##There is a character that influence tolower.
bible_ws_low <- tolower(bible_words)
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
#Create the vector contain the lowercase vocabulary.
b <- main_voc
#convey the main lowercase vocabulary into b.


bible_ws_upper <- grep("[ABCDEFGHIJKLMNOPQRSTUVWXYZ]",bible_words,value=TRUE)
#Find the words of uppercase.
uni_bible_ws <- unique(bible_ws_upper)
#Make the words of uppercase unique.
bible_ws_upper_low <- tolower(uni_bible_ws)
#Make them the lowercase.
uni_bible_ws[852]
up_main_word_pos <- match(bible_ws_upper_low,b)
# Match them with the main vocabulary.
upper_voc <- uni_bible_ws[-which(is.na(uni_bible_ws[up_main_word_pos]))]
#Find the uppercase words we need to include.


b <- append(b, upper_voc)
#Create the new b include the uppercase.


b_position <- match(bible_ws,b)
# Find the positions for each words for vector b.

follow_pri <- b_position[-1]
follow <- append(follow_pri, b_position[1])
#Eliminate the first word position and add it to the end in order to match the dimension.

pair_pos <- cbind(b_position, follow)
#Combine the first word and the word following into word pairs.



pair_pos <- pair_pos[-which(is.na(rowSums(pair_pos))),]
##delete the word contain NA to find the useful word pairs.  
A <- matrix(0,length(b),length(b)) 
#Create matrix A with all zeros.
for (i in 1:nrow(pair_pos )) {
#Loop through all the word pairs.  
  fir_pos <- pair_pos [i,1]
  #Find the position of the first word from the word pair.
  sec_pos <- pair_pos [i,2] 
  #Find the position of the second word from the word pair.
  A[fir_pos,sec_pos] <- A[fir_pos,sec_pos] + 1
  #Locate the word pair in A by their particular position and plus one once such word pair appears.
  
}

A <- A/rowSums(A)
#Make the sums of the rows of A into 1.
Ran_sentence <- array("",c(1,50))
#Create an array of 50.
Ran_sentence[1] <- sample(b,1)
#Randomly choose 1 word from the vocabulary.
for (i in 1:50) {
  
  Ran_sentence[i+1] <- sample(b,1,T,A[which(b==Ran_sentence[i]),])
  #Find the next word randomly from the particular probability.
}
cat(Ran_sentence)
#Show the words.


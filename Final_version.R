##Group:39
##
##name: Xiaoyi Zhou 
## Enlin Shen
## Lucie Pryke
##
##Id: s2190991
## s2191157
## s1812011 
setwd("put/your/local/repo/location/here")
text_bible <- scan("1581-0.txt", what="character", skip=156, encoding = "UTF-8")
#The code is changed a little bit here in case that strange unrecognizable characters appear.
n <- length(text_bible)
text_bible <- text_bible[-((n-2909):n)]; ## strip license

##split the punctuation from words
splitPunct <- function(text_0){
  
  #mark the punctuation by " " (add space before and after all the punctuation in order to split them)
  text_punctmark <- gsub('([[:punct:]])', ' \\1 ', text_0)
  
  #split the text by " " in ordre to get each characters
  text_split <- unlist (strsplit (text_punctmark, " ") )  
  
  return(text_split)
}

text_bible <- splitPunct(text_bible)

## Transform the words in bible into lowercase
text_bible_low <- tolower(text_bible)

##Find all the vocabulary in bible
text_unique <- unique(text_bible_low)

##Find the positions for each words
word_index <- match(text_bible,text_unique)

##Find the frequencies of each words
words_quantity <- tabulate(word_index)

##Find the top 1000 words
rank_1001 <- sort (words_quantity, decreasing = TRUE)[1001]
#word_1k becomes the main vocabulary
word_1k <- text_unique [which (words_quantity > rank_1001) ]

##Find the words of uppercase.
bible_ws_upper <- grep("^[A-Z]",text_bible,value=TRUE)

##Make the words of uppercase unique.
uni_bible_ws <- unique (bible_ws_upper)
##Make them the lowercase.
bible_ws_upper_low <- tolower (uni_bible_ws)
##Match them with the main vocabulary.
up_main_word_pos <- match (bible_ws_upper_low, word_1k)
##Find the uppercase words we need to include.
upper_voc <- uni_bible_ws [-which (is.na (uni_bible_ws [up_main_word_pos] ) )]

##Create the new word_1k include the uppercase.
word_1k <- append(word_1k, upper_voc)

##Find the positions for each words for vector word_1k.
word_match <- match(text_bible, word_1k)
##Combine the first word and the word following into word pairs.
##(Eliminate the final word for the first words 
##Eliminate the first word for the followed words in order to match the dimension)
word_matrix_pri <- cbind ( word_match [-length (word_match) ], word_match [-1])
##delete the word contain NA to find the useful word pairs. 
word_matrix <- word_matrix_pri [-which ( is.na ( rowSums (word_matrix_pri) ) ), ]
##Find the dimension of A
word_length <- length(word_1k)
##Create matrix A with all zeros.
A <- matrix(0, nrow = word_length, ncol = word_length)

for (i in 1:nrow(word_matrix)) {
  ##Loop through all the word pairs.
  
  fir_pos <- word_matrix [i,1]
  ##Find the position of the first word from the word pair.
  sec_pos <- word_matrix [i,2] 
  ##Find the position of the second word from the word pair.
  A[fir_pos,sec_pos] <- A[fir_pos,sec_pos] + 1
  ##Locate the word pair in A by their particular position and plus one once such word pair appears.
  
}


##Make the sums of the rows of A into 1.
A <- A / rowSums (A)
##Create an array of 50.
Ran_sentence <- array ("", c(1, 50))
##Randomly choose 1 word from the vocabulary.
Ran_sentence[1] <- sample (word_1k, 1)

for (i in 1:150) {
  
  ##Find the next word randomly from the particular probability.
  Ran_sentence [i+1] <- sample (word_1k, 1, T, A[which (word_1k == Ran_sentence [i]),])
  
}

##Show the words.
cat (Ran_sentence)






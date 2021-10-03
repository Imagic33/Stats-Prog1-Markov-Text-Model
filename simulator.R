Ran_sentence <- array("",c(1,100))
Ran_sentence[1] <- sample(word_1k,1)
for (i in 1:100) {
  
  Ran_sentence[i+1] <- sample(word_1k,1,T,A[which(word_1k==Ran_sentence[i]),])
  
}
cat(Ran_sentence)
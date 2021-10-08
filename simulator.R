Ran_sentence <- array("",c(1,50))
#Create an array of 50.
Ran_sentence[1] <- sample(b,1)
#Randomly choose 1 word from the vocabulary.
for (i in 1:50) {
  
    Ran_sentence[i+1] <- sample(b,1,T,A[which(b==Ran_sentence[i]),])
    #Find the next word randomly from the particular probability.
}
cat(Ran_sentence)
#Show thw words.
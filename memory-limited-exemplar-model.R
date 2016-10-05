# We've used the GCM model of categorization a number of times.
# One of the assumptions of GCM is that all of the category exemplars that you have
# ever seen equally influence the categorization decision. This seems like an unlikely
# claim, given what we know about human memory. 

# Implement a new categorization model, based on GCM, with an additional parameter
# for memory decay. Here's how the model should behave:

# - The data frame training.data will have columns x, y, and category. A sample data
#   frame is provided below. Run the code and open it to see what the data look like.
# - You can assume that the training.data are presented in order. Row 1 is the first
#   example seen, row 2 the second, and so on. That means that Row 1 will have decayed
#   the most relative to the other rows.
# - x.val and y.val are the values of the stimulus to be categorized on the two dimensions.
# - target.category is the label of the category that we want to know the classification probability
#   for.
# - sensitivity is the parameter that controls the relationship between distance and similarity.
# - decay.rate is a multiplier for the strength of encoding of an exemplar over time. For example,
#   if there are 10 stimuli in training.data then the first stimulus will have encoding strength
#   1*decay.rate^9. The second will be 1*decay.rate^8. And so on... The most recent stimulus should
#   be 1*decay.rate^0 (which is 1). This means that a decay.rate of 1 means no decay, and
#   a decay rate of 0 means that only the last. item has any effect on the categorization.
# - Your first step should be to calculate the weight for each item in training.data, and add that
#   weight to the training.data data frame.
# - Then you should calculate the Euclidean Distance between the target stimulus (x.val, y.val)
#   and each item in the training data.
# - Next, convert all the distances to similarities with the formula:
#   similarity = exp(-sensitivity*distance)
# - Then, multiply each similarity score by its weight to get the memory-weighted similarity.
# - Finally, add up all the similarity scores for the target category, and then divide by the
#   total similarity to get the predicted probability of a response for the target category.
# - Return this probability.

sample.training.data <- data.frame(x=c(0.5,0.6), y=c(0.4,0.3), category=c(1,2))

exemplar.memory.limited <- function(training.data, x.val, y.val, target.category, sensitivity, decay.rate){
    if(is.data.frame(training.data) && nrow(training.data)==0)){
      # above line taken from: http://stackoverflow.com/questions/28556658
      return(.5)
    }
    current.row <- length(training.data) + 1
    j <- 1
    while(j < current.row){
      training.data$weight[j] <- 1*(decay.rate^(current.row - j))
      training.data$distance[j] <- sqrt(abs(x.val-training.data$x[j])^2 + abs(y.val-training.data$y[j])^2)
      training.data$similarity[j] <- exp(-sensitivity * training.data$distance[j])
      training.data$mweighted.similarity[j] <- training.data$similarity[j] * training.data$weight[j]
      j <- j + 1
      }
    probability <- sum(training.data$mweighted.similarity) / sum(training.data$similarity)
    return(probability)
}
exemplar.memory.limited(sample.training.data, .5, .5, TRUE, .5, .5) 
# Once you have the model implemented, write the log-likelihood function for a set of data.
# The set of data for the model will look like this:

sample.data.set <- data.frame(x=c(0.5,0.6,0.4,0.5,0.3), y=c(0.4,0.3,0.6,0.4,0.5), category=c(1,2,2,1,2), correct=c(T,F,F,T,F))

# In our hypothetical experiment, we are training and testing at the same time. This is important
# for a model like this, because the model depends on the order in which examples are shown.
# It also means that you have to do a little work to separate the training and test data for each trial.
# If the subject is on trial 4 of the sample.data.set then the training data will be:

sample.data.set[0:3,]

# and the test item will be

sample.data.set[4,]

# So, you need to treat each row of all.data as a test item, and find the training set for it
# to give to your model. It may be easier to do this with a for loop than mapply(), though it
# is certainly possible with both. (For mapply, pass it the row number that you are on...)

# Don't forget that decay rate should be between 0 and 1, and that sensitivity should be > 0.

exemplar.memory.log.likelihood <- function(all.data, sensitivity, decay.rate){
  c.row <- 1
 for(i in all.data){
   all.data$probability <- exemplar.memory.limited(all.data[0:row.index,], all.data[c.row,]$x, all.data[c.row,]$y, all.data[c.row,]$category, sensitivity, decay.rate)
   row.index <- row.index + 1
   c.row <- c.row + 1
 }
 all.data$likelihood <- mapply(function(response, prob){
   
   if(response == TRUE){
     return(prob)}
   else{
     return(1-prob)}
 },
all.data$correct, all.data$probability)
 
 loglikelihood <- sum(log(all.data$likelihood))
 return(loglikelihood)
}

exemplar.memory.log.likelihood(sample.data.set, .5, .5)

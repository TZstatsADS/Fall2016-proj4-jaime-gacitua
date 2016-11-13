


testing.function <- function(word.models, test.set.data, word.columns, feature.columns,
                 encode.df){
  
  
  data.for.testing <- data.frame(x = test.set.data[,feature.columns])
  
  # Calculate predicted probabilities
  probabilities <- pblapply(1:length(word.columns), 
                            
                            function(x, word.models, data.for.testing){
                              #print(word.models[[x]])
                              
                              probabilities <- predict(word.models[[x]], 
                                                       newdata = data.for.testing, 
                                                       type = "response")                          
                              return(probabilities)
                            }
                            ,word.models = word.models
                            ,data.for.testing = data.for.testing
  )
  probability.mat <- unlist(probabilities) %>%
    matrix(ncol = num.words, byrow = FALSE)
  
  # Calculate complement to be able to apply ranking. 
  # Because when probability -> 1 ranking  -> +infty
  probability.mat <- 1-probability.mat
  
  # Generate Ranking
  ranking <- pbapply(probability.mat, 1,
                     function(x){
                       calc.rank <- rank(x, ties.method = "average")
                       return(calc.rank)
                     }
  )
  ranking <- t(ranking) # Songs in the rows, words in the columns
  
  
  
  # Plot sum of rankings for each word
  plot.sum.rankings(ranking, encode.df)
  
  
  # Calculate Predicive rank sum
  # The lower the better.
  
  rank.sum <- predictive.rank.sum(test.set.data, ranking, word.columns)
  
  hist(rank.sum$per.song,
       main = paste0("Predictive Rank Sum | Total = ",round(rank.sum$total, digits = 3)))
  
  
  return(rank.sum)  
}






predictive.rank.sum <- function(train.set.data, ranking, word.columns){
  
  K <- length(word.columns)
  
  num.songs <- nrow(ranking)
  
  word.columns.data <- train.set.data[,word.columns] %>%
    as.matrix()
  
  #word.columns.data <- as.double(word.columns.data)
  
  word.columns.data[word.columns.data >= 1] <- 1

  transposed.ranking <- t(ranking)
  
  # Calculate the result for every row
  ranksum.per.song <- lapply(1:nrow(ranking), 
                        function(x){
                          
                          # Calculate sum(  r_(w_i) )
                          sum.value <- sum(word.columns.data[x,]*transposed.ranking[,x])
                          
                          r.bar <- 1 / K * sum(transposed.ranking[,x])
                          m <- sum(word.columns.data[x,])
                          
                          return.value <- 1 / m / r.bar * sum.value
                          
                          return(return.value)
                          }
  )
  
  ranksum.per.song <- unlist(ranksum.per.song)
  
  ## Calculate total ranksum
  
  # Sum
  sum.value.total <- 0
  for(i in 1:num.songs){
    sum.value.total <- sum.value.total + sum(word.columns.data[i,]*transposed.ranking[,i])
  }
  
  # Other values
  r.bar.total <- sum(transposed.ranking) / K / num.songs
  m.total <- sum(word.columns.data)
  
  # Compute
  ranksum.total <- 1 / m.total / r.bar.total * sum.value.total
  
  
  return(list(total=ranksum.total, per.song=ranksum.per.song))
  
  
}

plot.sum.rankings <- function(ranking, encode.df){
  sum.ranks.word <- colSums(ranking)
  sum.ranks.word.names <- data.frame(word = encode.df$words, sum.rank = sum.ranks.word)
  sum.ranks.word.names <- sum.ranks.word.names %>% arrange(sum.rank)
  sum.ranks.word.names$word <- as.vector(sum.ranks.word.names$word)
  sum.ranks.word.names$word = factor(sum.ranks.word.names$word, sum.ranks.word.names$word)
  plot(x = sum.ranks.word.names$word, y = sum.ranks.word.names$sum.rank)
}




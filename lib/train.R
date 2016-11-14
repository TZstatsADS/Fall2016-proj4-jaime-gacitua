


train.model <- function(x, train.set.data, word.columns, all.columns){
  
  # Define columns to keep
  col.keep <- c(word.columns[x], feature.columns)
  
  # Define data to feed the function
  data.for.model <- train.set.data[,col.keep]
  
  # run logistic regression
  model <- run.logistic(data.for.model)
  return(model)
}
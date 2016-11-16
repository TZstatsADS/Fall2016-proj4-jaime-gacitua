


train.model <- function(x, train.set.data, word.columns, all.columns, model){
  
  # Define columns to keep
  col.keep <- c(word.columns[x], feature.columns)
  
  # Define data to feed the function
  data.for.model <- train.set.data[,col.keep]
  
  # run correspondign model
  if(model == 'logistic'){
    model <- run.logistic(data.for.model)    
  }
  else{
    model <- run.gbm(data.for.model)
  }
  return(model)
}
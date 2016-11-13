

run.logistic <- function(data.for.model){
  
  
  ## 1 ETL word column: binary values, and column name.
  names(data.for.model) <- c("y", "x")
  
  
  data.for.model[data.for.model[,1] >= 1,1] <- 1
  
  
#  model.formula <- paste0(current.word, " ~ ",feature.name)
#  model.formula <- as.formula(model.formula)
  #cat(model.formula)
  
  ## Perform logistic regression
  model <- glm(y ~ x, family = binomial(link = 'logit'), 
               data = data.for.model)
  
  return(model)
  
}

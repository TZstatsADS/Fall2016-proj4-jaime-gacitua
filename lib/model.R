

run.logistic <- function(data.for.model){
  
  num.columns <- ncol(data.for.model)
  
  num.features <- ncol(data.for.model) - 1 # first column is "word"
  
  ## 1 ETL word column: binary values, and column name.
  # Generate column names
  feature.names <- lapply(1:num.features, 
                         function(x){element <- paste0('x',x) }
  )
  feature.names <- unlist(feature.names)
  
  names(data.for.model) <- c("y", feature.names)
  
  data.for.model[data.for.model[,1] >= 1,1] <- 1
  
  features.text.formula <- "x1"
  if(num.features > 1){
    for(i in 2:num.features){
      features.text.formula <- paste0(features.text.formula, " + x",i)
    }
  }
  
  model.formula <- paste0("y ~ ",features.text.formula)
#  cat(model.formula)
  model.formula <- as.formula(model.formula)

  
  ## Perform logistic regression
  model <- glm(model.formula, family = binomial(link = 'logit'), 
               data = data.for.model)
  
  return(model)
  
}



run.gbm <- function(data.for.model){

  num.col <- ncol(data.for.model)
  
  data.for.model[data.for.model[,1] >= 1,1] <- 1
  
  data.features <- data.for.model[,2:num.col]
  data.label <- data.for.model[,1]

  
  fit.gbm <- gbm.fit(x=data.features, 
              y=data.label,
              n.trees= 800,
              distribution="bernoulli",
              interaction.depth=10, 
              bag.fraction = 0.5,
              verbose=FALSE)
      
    
  #cat("Finished fitting model", "\n")    

  #best.iter.gbm <- gbm.perf(fit.gbm, method="OOB")

  #cat("Finished tuning model", "\n")
  
  best.iter.gbm <- 2000
  
  return(list(trained.model=fit.gbm, best.param=best.iter.gbm))
}


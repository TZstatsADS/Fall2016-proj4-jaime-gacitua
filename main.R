rm(list=ls(all=TRUE))

library(readr)
library(plotly)
library(rhdf5)
library(dplyr)
library(pbapply)
library(rscopus)
source('./lib/features.R')
source('./lib/model.R')
source("./lib/functions.R")
source("./lib/test.R")


# Extract lyrics data
common_id <- read_csv('./data/common_id.txt', col_names = FALSE)

# Load Words Data and Filter unwanted words
load('./data/lyr.RData')
words.not.include <- c(2,3,6:30)
lyr <- lyr[,-words.not.include]
names(lyr)[1] <- 'song'

# Encode words to numbers to avoid strange characters
encode.df <- data.frame(words = names(lyr)[2:length(names(lyr))])
encode.df$code <- 1:nrow(encode.df)+100000
encode.df$code <- paste0(encode.df$code,'a')

names(lyr)[2:length(names(lyr))] <- encode.df$code

#####
# ETL features
#####

### Extract + transform Features
### This takes 5 minutes
dir.h5 <- './data/data/'
files.list <- as.matrix(list.files(dir.h5, recursive = TRUE))
song.features.df <- get.features(files.list, dir.h5)
######################

# combine words and features, to be sure songs are matching their features
song.words.features.df <- inner_join(x = lyr, y = song.features.df, 
                                     by = "song")




### Prepare test and training set ----

# Parameters
pct.train.set <- 0.8
set.seed(100)

# Calculations
num.songs <- nrow(lyr)
num.words <- ncol(lyr)-1
num.features <- ncol(song.features.df)-1
word.columns <- 1:num.words+1
feature.columns <- (1+num.words+1):(1+num.words+num.features)
all.columns <- c(word.columns, feature.columns)

train.set.size <- ceiling(pct.train.set*num.songs)





results <- lapply(1:5, function(x){
  # Sample index to use to train
  train.set <- sample(1:num.songs, train.set.size, replace=F)
  
  train.set.data <- song.words.features.df[train.set,]
  test.set.data <- song.words.features.df[-train.set,]
  
  #############
  ### Training Block
  #############
  
  ### Logistic regression over each word ----
  trained.models <- pblapply(1:length(word.columns), 
                             
                             function(x, train.set.data, word.columns, all.columns){
                               
                               # Define columns to keep
                               col.keep <- c(word.columns[x], feature.columns)
                               
                               # Define data to feed the function
                               data.for.model <- train.set.data[,col.keep]
                               
                               # run logistic regression
                               model <- run.logistic(data.for.model)
                               return(model)
                             }
                             ,train.set.data = train.set.data
                             ,word.columns = word.columns
                             ,all.columns = all.columns
                             
  )
  
  #######################
  ### Test Block
  #######################
  
  test.results <- testing.function(trained.models, test.set.data, word.columns, feature.columns,
                                   encode.df)  
  
  return(test.results$total)
}
)

results <- unlist(results)
hist(results)
mean(results)


### 0.223 result using only tempo








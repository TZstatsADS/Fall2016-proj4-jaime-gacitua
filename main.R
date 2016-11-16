rm(list=ls(all=TRUE))

library(readr)
library(plotly)
library(rhdf5)
library(dplyr)
library(pbapply)
library(rscopus)
library(gbm)
source('./lib/features.R')
source('./lib/model.R')
source("./lib/functions.R")
source("./lib/test.R")
source("./lib/train.R")


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

dir.h5 <- './data/data/'
files.list <- as.matrix(list.files(dir.h5, recursive = TRUE))
### This takes 5 minutes
song.features.df.1 <- get.features(files.list, dir.h5)
### This takes XX minutes
song.features.df.2 <- get.features.2(files.list, dir.h5)

### Select with one to use
song.features.df <- song.features.df.2
# combine words and features, to be sure songs are matching their features
song.words.features.df <- inner_join(x = lyr, y = song.features.df, 
                                     by = "song")

######################





### Prepare test and training set ----

# Parameters
pct.train.set <- 0.80 #####################################################3
set.seed(100)

# Calculations
num.songs <- nrow(lyr)
num.words <- ncol(lyr)-1
num.features <- ncol(song.features.df)-1
word.columns <- 1:num.words+1
feature.columns <- (1+num.words+1):(1+num.words+num.features)
all.columns <- c(word.columns, feature.columns)

train.set.size <- ceiling(pct.train.set*num.songs)


source('./lib/features.R')
source('./lib/model.R')
source("./lib/functions.R")
source("./lib/test.R")
source("./lib/train.R")


model = 'gbm'
results <- lapply(1:1, function(x){
  # Sample index to use to train
  train.set <- sample(1:num.songs, train.set.size, replace=F)
  
  train.set.data <- song.words.features.df[train.set,]
  test.set.data <- slice(song.words.features.df, -train.set)
  
  
  print(head(test.set.data[,feature.columns]))
  
  #############
  ### Training Block
  #############
  
  ### Logistic regression over each word ----
  trained.models <- pblapply(1:length(word.columns),
                             function(x)
                             train.model(x, train.set.data, word.columns, all.columns, model)
                             )
  
  #######################
  ### Test Block
  #######################
  
  print(trained.models[[1]])
  
  test.results <- testing.function(trained.models, test.set.data, word.columns, feature.columns,
                                   encode.df, model)  
  
  return(test.results$total)
})

results <- unlist(results)


#hist(results)
#mean(results)


### 0.250 result using tempo+var+tatum+var
### 0.241 result using only tempo
### 0.237 GBM 500

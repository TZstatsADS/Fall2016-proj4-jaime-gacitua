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
#lyr <- lyr[,-words.not.include]
head(names(lyr))
names(lyr)[1] <- 'song'

test.submission.data <- read.csv('./data/sample_submission.csv')

head(names(test.submission.data[-1]))

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
#song.features.df.1 <- get.features(files.list, dir.h5)
### This takes XX minutes
song.features.df.2 <- get.features.2(files.list, dir.h5)

### Select with one to use
song.features.df <- song.features.df.2
# combine words and features, to be sure songs are matching their features
song.words.features.df <- inner_join(x = lyr, y = song.features.df, 
                                     by = "song")

######################
summary(song.features.df)



source('./lib/features.R')
source('./lib/model.R')
source("./lib/functions.R")
source("./lib/test.R")
source("./lib/train.R")



### Prepare test and training set ----

# Parameters
pct.train.set <- 0.5 #####################################################3
set.seed(100)
num.folds <- 1

# Calculations
num.songs <- nrow(lyr)
num.words <- ncol(lyr)-1
num.features <- ncol(song.features.df)-1
word.columns <- 1:num.words+1
feature.columns <- (1+num.words+1):(1+num.words+num.features)
all.columns <- c(word.columns, feature.columns)

train.set.size <- ceiling(pct.train.set*num.songs)


model = 'gbm'
results <- lapply(1:num.folds, function(x){
  # Sample index to use to train
  train.set <- sample(1:num.songs, train.set.size, replace=F)
  
  train.set.data <- slice(song.words.features.df, train.set)
  test.set.data <- song.words.features.df[-train.set,]
  
  #############
  ### Training Block
  #############
  
  ### Train model over each word ----
  trained.models <- pblapply(1:length(word.columns),
                             function(x){
                               if(x %in% words.not.include){
                                 model <- NULL
                               } 
                               else {
                                 model <- train.model(x, train.set.data, word.columns, all.columns, model)  
                                 #model <- NULL
                                 }
                               return(model)
                             }
                             )
  #######################
  ### Test Block
  #######################
  
  print(trained.models[[1]])
  
  test.results <- testing.function(trained.models, test.set.data, word.columns, feature.columns,
                                   encode.df, model, words.not.include)  
  
  return(test.results$total)
})

results <- unlist(results)

#hist(results)
mean(results)

### 0.241 result using only tempo
### 0.250 result using tempo+var+tatum+var
### 0.245 tempo+var+tatum+var+key+loudness+duration
### 0.248 tempo+var+tatum+var+key+loudness+duration

### 0.237 tempo+var+tatum+var GBM 500 trees, 10 depth

### 0.237 tempo+var+tatum+var GBM 700 trees, 6 depth
### 0.237 tempo+var+tatum+var GBM 700 trees, 6 depth
### 0.229 tempo+var+tatum+var+key+loudness+duration+timbre GBM 200 trees, 7 depth
### 0.229 tempo+var+tatum+var+key+loudness+duration+timbre GBM 100 trees, 8 depth
### 0.234 avg tempo+var+tatum+var+key+loudness+duration+timbre GBM 100 trees, 8 depth
### 0.236 avg avg tempo+var+tatum+var+key+loudness+duration+timbre GBM 100 trees, 8 depth

source('./lib/features.R')
source("./lib/test.R")
dir.h5 <- './data/TestSongFile100/'
files.list.test <- as.matrix(list.files(dir.h5, recursive = TRUE))
song.features.df.test <- get.features.2(files.list.test, dir.h5, 1)


trained.models.all <- pblapply(1:length(word.columns),
                           function(x){
                             if(x %in% words.not.include){
                               model <- NULL
                             } 
                             else {
                               model <- train.model(x, song.words.features.df, 
                                                    word.columns, all.columns, model)  
                               #model <- NULL
                             }
                             return(model)
                           }
)

test.results.final <- testing.function(trained.models.all, song.features.df.test, 
                                       word.columns, feature.columns,
                                       encode.df, model, 
                                       words.not.include, only.predict = TRUE)  

test.results.final[1:100, 500:510]




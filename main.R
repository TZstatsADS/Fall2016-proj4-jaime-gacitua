library(readr)
library(plotly)
library(rhdf5)
library(dplyr)
source('./lib/features.R')

rm(list=ls(all=TRUE))

# Extract lyrics data
common_id <- read_csv('./data/common_id.txt', col_names = FALSE)
load('./data/lyr.RData')


# Explore lyrics words
frequencies <- data.frame('word' = names(lyr[,-1]), 'freq' = colSums(lyr[,-1]) )
frequencies <- frequencies[order(frequencies$freq, decreasing = TRUE),]
frequencies$word <- factor(frequencies$word, levels = frequencies$word)
frequencies$pct <- frequencies$freq / sum(frequencies$freq)

plot_ly(data = frequencies, x = ~word, y = ~freq, type = 'scatter', mode = 'markers',
        text = ~word) %>%
  layout(title = "Word frequency in songs",
         scene = list(
           xaxis = list(title = "Word",
                        type = 'category',
                        categoryorder = 'array',
                        categoryarray = ~word), 
           yaxis = list(title = "Count")))

plot_ly(data = frequencies, x = ~word, y = ~pct, type = 'scatter', mode = 'markers',
        text = ~word) %>%
  layout(title = "Word frequency in songs | Percentage",
         scene = list(
           xaxis = list(title = "Word",
                        type = 'category',
                        categoryorder = 'array',
                        categoryarray = ~word), 
           yaxis = list(title = "Percentage")))

head(frequencies, n = 50)


###########################################
# Understanding features of  h5 files   ###
# More info in:                         ###
# http://labrosa.ee.columbia.edu/millionsong/pages/example-track-description
###########################################


h5f <- h5dump('./data/data/A/A/A/TRAAABD128F429CF47.h5', load=TRUE)


analysis.test <- h5f$analysis
data.test <- h5f$data
metadata.test <- h5f$metadata
musicbrainz.test <- h5f$musicbrainz

# Tempo could help. Seems to fit with beats
beat.diff <- diff(analysis.test$beats_start,1)
hist(beat.diff)
bpm.test <- 1 / mean(beat.diff) * 60
bpm.test
test$songs$tempo

# Tatum
tatums.diff <- diff(test$tatums_start,1)
hist(tatums.diff)

H5close()


#####
# ETL features
#####

### Extract + transform Features


dir.h5 <- './data/data/'
files.list <- as.matrix(list.files(dir.h5, recursive = TRUE))

song.features <- get.features(files.list, dir.h5)


### Logistic regression over each word

## 1 Get word column

## 2 Merge column with features

## Perform logistic regression

## Understand output











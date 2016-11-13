
get.features <- function(files.list, directory){
  
  # counters to see progress
  num <- 0  
  total <- length(files.list)
  
    # Loop through all the data files, collect results as a list.
  features <- pblapply(files.list, function(x, dir){

    file <- paste0(dir,x)
    h5f <- h5dump(file, load = TRUE)
    analysis <- h5f$analysis
    tempo <- analysis$songs$tempo
    song <- substr(x, start = 7, stop = nchar(x)-3)
    H5close()

    return(c(song,tempo))
  },
  dir = directory
  )
  
  # Transform list into a data frame
  song.features.df <- unlist(features) %>% 
    matrix(byrow = TRUE, ncol = 2) %>%
    data.frame()
  names(song.features.df) <- c('song', 'tempo')
  song.features.df$tempo <- as.double(song.features.df$tempo)
  song.features.df$song <- as.character(song.features.df$song)


  return(song.features.df)
}

get.features <- function(files.list, directory){
  

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

get.features.2 <- function(files.list, directory){
  # We now get 4 features:
  #                       + Tempo
  #                       + Variance of tempo
  #                       + Average Tatums
  #                       + Variance of Tatums
  
  
  # Loop through all the data files, collect results as a list.
  features <- pblapply(files.list, function(x, dir){
    
    file <- paste0(dir,x)
    h5f <- h5dump(file, load = TRUE)
    analysis <- h5f$analysis
    
    beat.diff <- diff(analysis$beats_start,1)
    tatums.diff <- diff(analysis$tatums_start,1)

    tempo <- analysis$songs$tempo
    tempo.var <- var(60 / beat.diff)
    tatums.mean <- mean(tatums.diff)
    tatums.var <- var(tatums.diff)
    
    
    
    song <- substr(x, start = 7, stop = nchar(x)-3)
    H5close()
    
    return(c(song, tempo, tempo.var, tatums.mean, tatums.var))
  },
  dir = directory
  )
  
  # Transform list into a data frame
  song.features.df <- unlist(features) %>% 
    matrix(byrow = TRUE, ncol = 5) %>%
    data.frame()
  names(song.features.df) <- c('song', 'tempo', 'tempo.var', 'tatums', 'tatums.var')

  song.features.df$song <- as.character(song.features.df$song)
  song.features.df$tempo <- as.double(song.features.df$tempo)
  song.features.df$tempo.var <- as.double(song.features.df$tempo.var)
  song.features.df$tatums <- as.double(song.features.df$tatums)
  song.features.df$tatums.var <- as.double(song.features.df$tatums.var)
  
  
  return(song.features.df)
}

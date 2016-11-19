
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

get.features.2 <- function(files.list, directory, start.txt = 7){
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

#    tempo <- analysis$songs$tempo
    tempo <- median(beat.diff)
    tempo.var <- var(60 / beat.diff)
    tatums.mean <- mean(tatums.diff)
    tatums.var <- var(tatums.diff)
    
#    key <- analysis$songs$key
#    loudness <- analysis$songs$loudness
    loudness.mean <- mean(analysis$segments_loudness_max)
    loudness.var <- var(analysis$segments_loudness_max)
    
#    duration <- analysis$songs$duration
    duration <- max(analysis$beats_start)+mean(beat.diff)
    
    # Fix cases of null
#    if(is.null(key)){ key <- 0 }
    if(is.null(loudness.mean)){ loudness.mean <- 0 }
    if(is.null(loudness.var)){ loudness.var <- 0 }
    if(is.null(duration)){ duration <- 0 }
    
    song <- substr(x, start = start.txt, stop = nchar(x)-3)
    
    timbre <- apply(analysis$segments_timbre, 1, function(x){
      median(x)
    } )
    timbre <- t(timbre)
    
    H5close()
    
    return(c(song, tempo, tempo.var, tatums.mean, tatums.var, 
             loudness.mean, loudness.var, duration, unlist(timbre)))
  },
  dir = directory
  )
  
  num.columns <- 20
  # Transform list into a data frame
  song.features.df <- unlist(features) %>% 
    matrix(byrow = TRUE, ncol = num.columns) %>%
    data.frame()
  
  timbre.names <- lapply(1:12, paste0, "timbre")
  timbre.names <- unlist(timbre.names)
  
  names(song.features.df) <- c('song', 'tempo', 'tempo.var', 'tatums', 'tatums.var',
                               'loudness.mean', 'loudness.var', 'duration', unlist(timbre.names))
  

  song.features.df[is.na(song.features.df)] <- 0
  
  ## Convert into meaningful formats
  song.features.df$song <- as.character(song.features.df$song)
  song.features.df[2:num.columns] <- lapply(song.features.df[2:num.columns], 
                                               function(x) as.numeric(as.character(x)))
  
  
  
  return(song.features.df)
}

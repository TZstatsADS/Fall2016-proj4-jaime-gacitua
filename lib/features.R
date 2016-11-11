
get.features <- function(files.list, directory){
  
  features <- lapply(files.list, function(x, dir){
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
  song.features.df <- unlist(song.features)
  song.features.df <- matrix(song.features.df, ncol = 2, byrow = TRUE)
  song.features.df <- data.frame(song.features.df)
  names(song.features.df) <- c('song', 'tempo')
  song.features.df$tempo <- as.double(song.features.df$tempo)
#  hist(song.features.df$tempo)
  
  return(features)
}
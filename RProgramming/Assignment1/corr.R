corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # List of all .csv files in directory
  files <- dir(directory, pattern = '*.csv', full.names = TRUE)
  
  com <- complete(directory)
  gtThresh <- com[com['nobs'] > threshold, 'id']
  
  cors <- vector(mode='numeric', length=0)
  
  for(i in gtThresh){
    file <-read.csv(files[i])
    cleanFile <-file[complete.cases(file),]
    cors <- c(cors, cor(cleanFile['sulfate'],cleanFile['nitrate']))
  }
  cors
}
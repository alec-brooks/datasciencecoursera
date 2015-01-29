pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # List of all .csv files in directory
  files <- dir(directory, pattern = '*.csv', full.names = TRUE)
  
  #Build vector of pollutants from desired files
  pollutants <- c()
 
  for(i in id){
    pollutants <- c(pollutants, read.csv(files[i])[[pollutant]])
  }
  
  #Returns mean of clean data
  mean(pollutants[!is.na(pollutants)])
  
  
}
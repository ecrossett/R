complete <- function(directory, id = 1:332) {
  # This function reads a directory full of files and reports the number of
  # completely observed cases in each data file. Returns a data frame
  # where the first column is the name of the file and the second column
  # is the number of complete cases.
  
  # Initialize the filename and Observation column headers as empty vectors
  ids = c()
  obs = c()
  # Get a list of all files in the directory
  files = list.files(directory)
  
  # Loop through each input filenumber (id) and read each csv file
  for(i in id) {
    location = paste0(directory,"/", files[i])
    readData = read.csv(location, header = TRUE)
    # Get a subset of all rows with complete data (ie: no NAs)
    # completeCases = subset(readData, !is.na(Date) & !is.na(sulfate) & !is.na(nitrate) & !is.na(id), select = TRUE)
    completeCases = readData[complete.cases(readData), ]
    # Add each input ID and new observation count to the list
    ids = c(ids,i)
    obs = c(obs, nrow(completeCases))
  }
  # Return values in a data frame
  data.frame(ids,nobs = obs)
}

## This function returns the mean of a 'pollutant' across monitor IDs specified in id vector for files from the specified 'directory'
pollutantmean <- function(directory, pollutant, id = 1:332) 
{
  ## Get a list of files from the specified directory and add them a list
  files_list <- list.files(directory, full.names=TRUE)
  
  ## Create an empty data frame to hold data frames from each of the above files. 
  multiDataFrames <- vector(mode="list", length=length(files_list))
  
  ## Add each file as a separate data frame
  for(i in seq_along(files_list))
  {
    multiDataFrames[[i]] <- read.csv(files_list[[i]]) 
  }
  
  ## Combine all data frames into one big data frame
  allDataDataFrame <- do.call(rbind, multiDataFrames)
  
  ## Create a new data frame to hold data only for the specified monitor IDs from the id vector function input
  newDataFrames <- vector(mode="list", length=length(id))
  
  ## Use the id vector input to subset the data for only desired ids from the all data frame. Each id will be added as a spearate data frame
  for (j in seq_along(id))
  {
    newDataFrames[[j]] <- allDataDataFrame[which(allDataDataFrame$ID == id[[j]]), ] ## Use the id specified in the id vector input to match the ids in the all data frame
  }
  
  ## Combine all the desired ids into one big data frame
  desiredIDsDataFrame<-do.call(rbind, newDataFrames) 
  
  ## Find the mean of the desired pollutant from the specified pollutant input to the function
  mean(desiredIDsDataFrame[,pollutant], na.rm=TRUE)
}

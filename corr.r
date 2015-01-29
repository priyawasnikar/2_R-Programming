## This function finds out the correlation between the 2 pollutants (sulfate, nitrate) for files where the #valid observations is > threshold specified
corr <- function(directory, threshold=0) 
{
  ## Get a list of files from the specified directory and add them a list
  files_list <- list.files(directory, full.names=TRUE)

  ## Create an empty data frame to hold data frames from each of the above files. 
  multiDataFrames <- vector(mode="list", length=length(files_list))
  
  ## Create a vector that will be returned. Each element in this vector will be a correlation value for the file that met the threshold criteria
  corVector <- vector(mode="integer")
  
  ## increment this number to keep adding elements to the vector to be returned
  numCorVector <- 1
  
  ## Loop through all the files in the directory
  for(i in seq_along(files_list))
  {
    
	## Read each file as a data frame
    multiDataFrames[[i]] <- read.csv(files_list[[i]]) 
	
	## Assign it to a new data frame for ease of referencing
	newDF <- multiDataFrames[[i]]
	
	## Find out the #valid observations 
    good <- complete.cases(newDF)
	
	## Get the number of rows with the valid observations, i.e. the number of valid observations 
	completeThres <- nrow(newDF[good,][,])
	
    #cat("completeThres: ", completeThres, "\t")
	## Compare # valid observations in current file/data frame to the threshold
	if(completeThres > threshold)
	{
		## if #Valid > threshold, create a ne data frame with just the valid values
		goodDF<-newDF[good,][,]
		
		## Find out the correlation between sulfate and nitrate and add it to the vector to be returned
		corVector[numCorVector] <- cor(goodDF$sulfate, goodDF$nitrate)
		
		## Increment the pointer so that the next valid value can be added the vector to be returned
		numCorVector <- numCorVector+1
	}
  }
  ## return the correlation vector
  return (corVector)
}

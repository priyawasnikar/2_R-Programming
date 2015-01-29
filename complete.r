## This function takes in a list of monitor ids as input from the directory specified, and returns a data frame that displays the file ID and the number of valid observations seen in each file
complete <-function(directory, id=1:332)
{
  ## Number of columns for the data frame being returned
  numCols=2
  
  ## Get a list of files from the specified directory and add them a list
  files_list <- list.files(directory, full.names=TRUE)
  
  ## Create an empty data frame to hold data frames from each of the above files. 
  multiDataFrames <- vector(mode="list", length=length(files_list))
  
  ## Create a vector to hold the file id 
  FileID<-vector(mode="numeric")
  
  ## Create a vector to hold the number of valid observations in each of the files being audited
  nobs<-vector(mode="numeric")
  
  ## Loop over the number of files from the id input vector
  for(i in 1:length(id))
  {
    ## Add each file as a data frame. Assign it to a new data frame newDF for ease of referencing 
	multiDataFrames[[i]] <- read.csv(files_list[[id[[i]]]]) 
    newDF<-multiDataFrames[[i]]
    
	## Find out the valid observations in the new data frame
	good<- complete.cases(newDF)
	
	## Add the file id to the file id vector for the returning data frame
    FileID[i]<-id[[i]]
	
	## Add the number of "good" rows/valid observations to the data frame being returned
    nobs[i]<-nrow(newDF[good,][,])
  }
  
  ## Once looping is complete, create a data frame to return and add the file ID and number of observations from the vectors above
  completeDF<- data.frame(FileID,nobs)
}
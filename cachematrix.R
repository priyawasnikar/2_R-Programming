## This function will take a matrix and return a matrix object with functions to set/initialize 
## the matrix, get the value for the matrix, set the inverse of the matrix and get the inverse of 
## the matrix (at the console as well as check the cached value for cacheSovle)

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse to be null
  inv <- NULL  
  
  setMat <- function(y){
    
    ## set the matrix to the new data set. 
    ## can be done from the console for example: >inputMatrix$setMat(matrix(1:4, 2,2))
    x <<- y
    
    ## When the user calls the setMat function and a new matrix is being created, inverse is set to NULL. 
    ## This prevents cacheSolve from using a cached value for an old matrix and forces cacheSolve to calculate the inverse
    inv <<- NULL
  }
  
  ## Calling getMat on the matrix object. 
  ## For example: at the console >inputMatrix$getMat() and the current matrix data will be printed
  getMat <- function() x
  
  ## Calling the setMat will allow cacheSolve to set the inverse value. Also can be used at the console, for example: >inputMatrix$setInv(solve(inputMatrix))
  setInv <- function(invValue)
        inv <<- invValue
  
  ## cacheSolve will call this function to check if there is a value for inverse of the matrix. 
  ## Also, user can call this function on the console to get the inverse for: >inputMatrix$getInv()
  getInv <-function() inv
  
  ## matrix object returns a list containing functions to set/get the matrix, 
  ## and set/get the matrix inverse
  list(setMat=setMat , getMat=getMat, setInv=setInv, getInv=getInv)
}

## cacheSolve accepts a matrix object created from makeCacheMatrix above. 
## It checks if there is an inverse value for the matrix dataset. If so, it returns the cached 
## inverse value, and if not, it calculates the inverse, and sets the value so that for the same dataset the cached value can now be used

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)){
    message("Getting cached inverse value")
    return(inv)
  }
  ## If the inverse value is null and inverse needs to be calculated, 
  ## this will grab the latest matrix data set
  matData <- x$getMat()
  
  ## calculate the inverse on the new matrix data set
  inv <- solve(matData)
  
  ## set the inverse for the new matrix data set
  x$setInv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}

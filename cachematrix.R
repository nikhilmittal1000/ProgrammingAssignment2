## makeCacheMatrix will create a list that contains functions to
## get the input matrix, and to get and set the inverse of this matrix from cache

## cacheSolve function will use makeCacheMatrix to check for existance of matrix inverse in cache 
## else it will create inverse of the matrix and store in cache using makeCacheMatrix


# Function returns list containing functions for gettng data, getting and setting inverse from cache
makeCacheMatrix <- function(x = matrix()) {
  
# check if objects exists or else create them as null in global environment
  if(!exists("cachedMatrix")) cachedMatrix <<- NULL
  if(!exists("cachedInv")) cachedInv <<- NULL
  
  getData <- function() x

  setInverse <- function(inputMatrix,inverseMatrix) {
    cachedMatrix <<- inputMatrix
    cachedInv <<- inverseMatrix
  }
  
  getInverse <- function(inputMatrix) {

# Check if input matrix is same as the cached one
    if(dim(inputMatrix)==dim(cachedMatrix) && all(inputMatrix == cachedMatrix)) 
      return(cachedInv)
  }

  list(getData = getData, setInverse = setInverse, getInverse = getInverse)
}



## Function will check cache for inverse, else create it and store
cacheSolve <- function(x, ...) {
  data <- x$getData()
  
  cachedInv <- x$getInverse(data)
  if(!is.null(cachedInv)) {
    message("getting cached inverse")
    return(cachedInv)
  }
  
  message("calculating inverse")
  inverseMatrix <- solve(data, ...)
  x$setInverse(data,inverseMatrix)
  inverseMatrix
}

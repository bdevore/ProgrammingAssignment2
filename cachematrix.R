## Functions for performing matrix investion. Includes a caching matrix object that can
## be used for increasing performance for the expensive matrix inversion operation

## Creates special matrix object that is able to cache its inverse for 
## improved performance
makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return the inverse of the supplied matrix, using the cached matrix object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  } 
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

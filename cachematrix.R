## this function creates a matrix object and can cache the inverse of said matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## setting the value of the matrix
  set <- function(y) {
    ## <<- for global variables so they apply across functions
    x <<- y
    inverse <<- NULL
  }
  ## getting the value of the matrix 
  get <- function() x
  
  ## setting the inverse function to apply across functions
  setinverse <- function(inverse) {
    inverse <<- inverse
  }
  ## defining the function that calculates the inverse of the matrix and
  ## caches the result
  
  inverse_cache <- function() {
    if(!is.null(inverse)) {
      message("getting cached data...")
      return(inverse)
    }
    message("calculating inverse...")
    inverse <- solve(x)
    setinverse(inverse)
    inverse
  }
  list(set = set, get = get, inverse_cache = inverse_cache, setinverse = setinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$inverse_cache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinverse(m)
  m
}

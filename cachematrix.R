## These functions create a special object that stores a 
## matrix and cache's its inverse

## Basic functions for matrix, get, set and inverse
## set and get

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # first set inverse to NULL, not defined yet
  set <- function(y) {
    x <<- y # initialize stored "x" matrix and inverse
    inv <<- NULL
  }
  get <- function() x # just returns stored matrix
  setinverse <- function(inverse) inv <<- inverse
  # stores inverse
  getinverse <- function() inv # gets stored inverse
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
} # returns list of functions to new "matrix"


## returns stored inverse if it exists, calculates otherwise

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # NULL if not already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # returns it if cached and shows message
  }
  data <- x$get() # returns cached vector
  inv <- solve(data, ...) # finds inverse of it ...
  x$setinverse(inv) # then sets it in cache for next time :-)
  inv # return it!
}

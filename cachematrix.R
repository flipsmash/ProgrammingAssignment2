## Functions to enable the cacheing of
## matrix inversion

## makeCacheMatrix take a matrix and returns a list of functions 
## to get & set the matrix and its inverse for use in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set <- function(y) {
    x <<- y
    i<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i<<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix
## if that inverse does not already exist in memory
## and returns the cached value of that inverse if it does

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve (data, ...)
  x$setinverse(i)
  i
}

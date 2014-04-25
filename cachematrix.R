## These functions are meant to save computing time by caching the 
## computed inverse of a matrix instead of repeating the computation
## multiple times

## The makeCacheMatric function creates a matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The cacheSolve function uses the R "solve" function to compute
## the inverse of the matrix created above. It retrieves said inverse
## from cache if it has already been calculated

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

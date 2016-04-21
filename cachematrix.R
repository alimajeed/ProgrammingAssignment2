
## Caching the Inverse of a Matrix.
## makeCacheMatrix stores a matrix and it's inverse
## cacheSolve computes the inverse if not already cached

## Following function creates a list of function to apply on stored matrix

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Following function calcuates the inverse of matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  cachedInverse <- x$getInverse()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  data <- x$getMatrix()
  cachedInverse <- solve(data, ...)
  x$setInverse(cachedInverse)
  cachedInverse
}
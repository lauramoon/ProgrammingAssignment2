## These two functions cache the inverse of a matrix
## to avoid having to compute it multiple times
## while executing R code

## This function ceates a special "matrix" object
## that can cache its inverse. It is actually a list of
## functions that tracks a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the "matrix"
## created by makeCacheMatrix, unless the inverse has already
## been calculated, in which case, it retrieves the cached value

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}

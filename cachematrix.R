## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. 
## This pair of functions cache the inverse of a matrix & create special object for that.


## This function creates a special "matrix" object that can cache its inverse.
# Object contains functions: set,get, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse_matrix) inverse <<- inverse_matrix
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

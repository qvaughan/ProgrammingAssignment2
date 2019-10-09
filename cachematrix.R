## Functions to support caching the inverse of a matrix.

## Returns a List object that contains the following functions:
## get(): Return the original matrix
## set(x): Set the original matrix 
## getinverse(): Return the cached inverse
## setinverse(inverse): Set the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the matrix found in the given argument. The argument should
## be a cacheable matrix as created from the makeCacheMatrix() function. If the inverse is
## already found in the cache, the cached inverse will be returned. If the inverse is not cached, it will 
## compute the inverse, save it to the cache, then return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

# Assignment 2

## The following functions makeCacheMatrix and cacheSolve cache the inverse of a given matrix.
## Both these functions assume that the matrix supplied is always invertible.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  return(inv)
}

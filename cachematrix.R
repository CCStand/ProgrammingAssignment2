## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix:
## Creates a special 'makeCacheMatrix' object that stores the value of a matrix and the value of the matrix's inverse
## With this object, these values can be altered (via the set and setInverse procedures)
## These values can then be retreived from the object (via the get and getInverse functions)
makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  getInverse <- function() {
    inverse
  }
  
  ## This code allows the functions to be referred to by name in the parent environment
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cahceSolve:
## If the inverse matrix of the given 'makeCacheMatrix' object has already been cached, this value is returned
## Otherwise, the inverse of the matrix is calculated and cahced in the given 'makeCacheMatrix' object
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

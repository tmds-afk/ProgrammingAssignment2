## This R Script allows to automate calculations
## that comes in handy while working with big arrays of data.
## In this task it helps to cache  matrices' contents.

##makeCacheMatrix() is a function that creates a matrix 
## and is capable of setting/getting its values as well as inverting them

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##cacheSolve() computes inverse of the matrix
## using arguments of makeCacheMatrix()
## message("getting cached data") pops up if inverse of the matrix
## has been already calculated and called by user

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
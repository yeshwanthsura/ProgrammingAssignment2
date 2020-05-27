## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A helper function that returns a list of functions to be used by @cacheSolve function

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(m) {
    mat <<- m
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
## This function tries to get the inv of a matrix if it is already present,
## or calculates the inverse and stores it for future  use, if not present

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinv(i)
  i
}

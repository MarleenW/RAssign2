## Submission to the R programming course on coursera.org
##
## 9-17-2014
##
## These functions provide an efficient way to compute the inverse of a matrix
##  by storing the inverse in cache after the first computation
## cacheSolve(makeCacheMatrix(m)) returns the inverse of matrix m.

## Creates a special matrix from a simple matrix which is a list of getter/setter functions 
##  for the simple matrix and it's inverse
## The inverse matrix is placed in cache to avoid costly re-computation
makeCacheMatrix <- function(matr = matrix()) {
  inverse <- NULL
  set <- function(m) {
    matr <<- m
    inverse <<- NULL # clear the cache of inverses of previous matrices
  }
  get <- function() matr
  setInverse <- function(i) inverse <<- i # save the inverse in cache
  getInverse <- function() inverse
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculates the inverse of a matrix of the form made by makeCacheMatrix
## Returns a simple matrix
cacheSolve <- function(mFast, ...) {
  inverse <- mFast$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- mFast$get()
  inverse <- solve(data) # creates inverse if not already present in cache
  mFast$setInverse(inverse) # stores computed inverse in cache
  inverse
}

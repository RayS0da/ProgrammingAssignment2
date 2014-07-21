## The following two functions cache the inverse of a matrix.


## This function, makeCacheMatrix creates a special "matrix" object that can cache its inverse,
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # initialize variable inv with NULL
  inv <- NULL
  
  # set the value of matrix x and initialize variable inv with NULL
  setMatrix <- function(y)  {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of matrix x
  getMatrix <- function()
    x
  
  # set the value of matrix inv
  setInverse <- function(inverse)
    inv <<- inverse
  
  # get the value of matrix inv
  getInverse <- function()
    inv
  
  # create a list containing a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function, cacheSolve computes the inverse of the special "matrix" returned by the above
## function makeCacheMatrix. It first checks to see if the inverse has already been calculated
## (and the matrix has not changed). If so, it gets the inverse of the matrix from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matrix and sets the inverse
## of the matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  # check to see if the inverse has been calculated
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    # if so, return to the inverse matrix has been calculated
    message("getting cached data")
    inv
  } else {
    # otherwise, calculate the inverse matrix via the setInverse function
    dataMatrix <- x$getMatrix()
    inv <- solve(dataMatrix)
    x$setInverse(inv)
    inv
  }
}
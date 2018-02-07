## makeCacheMatrix creates a special matrix object which is a list
## of functions to set and get a matrix and also set and get an inverse of the matrix
## cacheSolve returns the inverse of the special matrix created
## by makeCacheMatrix if it exists and computes and returns the inverse of the matrix if it doesn't already exist


## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize variable for mInverse
  mInverse <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  
  ## get the value of the matrix
  getMatrix <- function() x
  
  ## set the value of the inverse of the matrix
  setInverse <- function(mI) mInverse <<- mI
  
  ## get the value of the inverse of the matrix
  getInverse <- function() mInverse
  
  ## return a list of the functions to set and get the value of a matrix
  ## and functions to set and get the value of an inverse of the matrix
  list(set = set, getMatrix = getMatrix, setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed),
## retrieve the inverse from the cache and return it.
cacheSolve <- function(x, ...) {
  
  ## get inverse of x
  mInverse <- x$getInverse()
  
  ## if inverse of x already exists return it
  if(!is.null(mInverse)){
    message("getting cached data")
    return (mInverse)
  }
  ## else if inverse of x doesn't exist, 
  ## compute,cache and return the inverse
  matrix <- x$getMatrix()
  mInverse <- solve(matrix)
  x$setInverse(mInverse)
  mInverse
}



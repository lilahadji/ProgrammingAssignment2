## Lila Hadji - Data Science/R programming -October 2014
##    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
##    § Programming Assignment 2 - Week3.                                                               §
##    § Below are two functions that are used to create a special object that stores a square matrix    §
##    § and caches its inverse.                                                                         §
##    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The first function, makeCacheMatrix creates a special "matrix"object that can cache its inverse.
## The Object is a list containing a function to
#     1. set the value of the matrix
#     2. get the value of the matrix
#     3. set the value of the inverse
#     4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  ##Initialize the inverse matrix to NULL when creating a new matrix object
  ix <- NULL
  set <- function(y) {
    ##set the matrix object with y, and reset the cached inverse matrix
    x <<- y
    ix <<- NULL
  }
  get <- function() x ## this function returns the matrix value
  setinverse <- function(inv) ix <<- inv ## this function sets the cached inverse matrix
  getinverse <- function() ix ## this function returns the cached inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. when the inverse matrix has already been calculated, 
## the function gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
## the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ix <- x$getinverse()
  ## this instruction checks whether or not the inverse has already been calculated
  if(!is.null(ix)) { 
    message("getting cached data")
    ##if the invrse is the cache, the function skips the calculation and retrieves
    ##the value in the cache
    return(ix) 
  }
  ## This code is executed only when the inverse hasn't been set already. 
  data <- x$get() ##retrieve the matrix value
  ix <- solve(data, ...)## In this case, the function calculates the inverse
  x$setinverse(ix)## and caches it.
  ix ## return the inverse matrix
  
}

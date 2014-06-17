## This function library provided two functions to assist in the computation
## of the inverse of square matrix with the ability to cache previously determined
## to void the costly computation associated

## Usage Example:
## aMatrix <- stats::rnorm(16)
## dim(aMatrix) <- c(4,4)
## aCacheMatrix <- makeCacheMatrix(aMatrix)
## cacheSolve(aCacheMatrix)

## This function create custom matrix object that can cache the inversion of the
## matrix. Note this function expects a square numeric matrix 
## TODO: Check the matrix is a square matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) i <<- solve
  getSolve <- function() i
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## This function determines the inverse of a CacheMatrix or return previously determined
## inverse that has been cached
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getSolve()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setSolve(i)
  i
}

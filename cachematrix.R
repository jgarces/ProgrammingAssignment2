## This function library provides two functions to assist in the computation
## of the inverse of square matrix with the ability to cache previously determined
## to void the costly computation associated

## Usage Example:
## aMatrix <- stats::rnorm(16)
## dim(aMatrix) <- c(4,4)
## aCacheMatrix <- makeCacheMatrix(aMatrix)
## cacheSolve(aCacheMatrix) ## Calling cacheSolve(makeCacheMatrix(aMatrix)) produces a error
## cacheSolve(aCacheMatrix) ## Call a second time to confirm cache is returned
## aMatrix %*% cacheSolve(aCacheMatrix) ## Confirm matrix returned is inverse

## Test a noninvertable Matrix error Handling
## aSingularMatrix <- c(2,1,6,3)
## dim(aSingularMatrix) <- c(2,2)
## aCacheMatrix <- makeCacheMatrix(aSingularMatrix)
## cacheSolve(aCacheMatrix)

## Test a non square matrix error handling
## aNonSquareMatrix <- c(1,2,3,4,5,6)
## dim(aNonSquareMatrix) <- c(3,2)
## aCacheMatrix <- makeCacheMatrix(aNonSquareMatrix)
## cacheSolve(aCacheMatrix)

## This function create custom matrix object that can cache the inversion of the
## matrix. 
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
## inverse that has been cached. Note this function expects a square numeric matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixDimensions <- dim(x$get())
  if (length(matrixDimensions) == 2 && matrixDimensions[1] == matrixDimensions[2] ){
    i <- x$getSolve()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- tryCatch({
      solve(data, ...)  
    },warning = function(war){
      #Catch warning
    },error = function(err){
      #Catch error
      # If error message from solve contains singular then supplied matrix is noninvertable
      if (grepl("singular", err)){
        "Supplied matrix is noninvertible"
      }
      else{
        err
      }        
    }, finally = {
      #Finally
    })
    x$setSolve(i)
    i    
  }
  else{
    print("Supplied matrix is not a square matrix")
  }
}

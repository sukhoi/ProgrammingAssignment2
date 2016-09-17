## Contains 2 functions for computing the inverse of a matrix A using <<- 
##
## > A = matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3,byrow=TRUE)
## > x <-  makeCacheMatrix(A)
## > A_dash <- cacheSolve(x)
## > print(A)
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
##


## function: makeCacheMatrix:
##
## Description: This function creates a special "matrix" object that can cache its inverse.
##
## Input Arguments: x : the original matrix
## Returns : list of function objects
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  ## function: set
  ## Description: Sets the original matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## function: get
  ## Description: Gets the original matrix
  get <- function() x

  ## function: setInvMat
  ## Description: Sets the Inverse of the Matrix
  setInvMat <- function(invMat) m <<- invMat

  ## function: getInvMat
  ## Description: Gets the Inverse of the Matrix
  getInvMat <- function() m

  ## list of method/function objects
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}


## function: cacheSolve
##
## Description:  This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
##
## Input Arguments : x : list of the function objects
## Returns : m : inverse of the matrix
##
cacheSolve <- function(x, ...) {
  ## gets the inverse of the matrix
  m <- x$getInvMat()

  ## if already calulated, return the inverse of the matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  ## gets the orignal matrix
  data <- x$get()

  ## computes the inverse using the solve function
  m <- solve(data,...)

  ## sets the value using the setInvMat function
  x$setInvMat(m)

  ## Returns the inverse of the Matrix
  m
}

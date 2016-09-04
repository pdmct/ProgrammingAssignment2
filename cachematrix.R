## cacheMatrix.R
##
## Peter McTaggart
## 4//9/2016
##
## creates a special matrix type and associated functions that 
## will cache the results of the inverse operation
## and a function to get the cached result if the matrix data hasn't changed/
## (or compute the inverse if it hasn't been computed before)
## this should ensure that unnecessary inverse computation are avoided.
 

## Create the cacheMatrix type from a supplied matrix
## ARGS:  x : matrix()
## RETURNS:  cacheMatrix (a list of operations on the cachMatrix)
## ASSUMPTIONS: matrix is always invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL   ## removes previously cached value since we have new data for x
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## Solves for the inverse of the supplied cacheMatrix
## if this inverse has already been cached and the data hasn't changed it will
## return the cached value, otherwise it will calculate the inverse and return it
## ARGS:  x : a cacheMatrix
## RETURNS: inverse of X (using solve) or NA
## ASSUMPTIONS: matrix is invertible, with return NA if matrix is singular

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    ## return the already calculated inverse
    print("getting cached data")
    return(inv)
  }
  ## otherwise calcualte the inverse, store it and return it
  data <- x$get()
  ## test if matrix is singular by trying to solve for the inverse and catch any errors: 
  ## (from http://stackoverflow.com/questions/24961983/how-to-check-if-a-matrix-has-an-inverse-in-the-r-language)
  ## could also consider the matrixcalc package is.singular.matrix(x) fn.
  if( class(try(inv <- solve(data),silent=T))=="matrix") {
    x$setinv(inv)
    return(inv)
  } else {
    warning("Matrix is singular : inverse is NA")
    return(NA)
  }
}


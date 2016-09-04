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
## RETURNS: inverse of X (using solve)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    ## return the already calculated inverse
    return(inv)
  }
  ## otherwise calcualte the inverse, store it and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}



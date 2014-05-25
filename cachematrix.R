## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## cached inverse -- initialize to NULL
  mInv <- NULL
  
  ## getter
  get <- function() x
  
  ## setter
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  
  ## getter for the inverse
  getInverse <- function() mInv
  
  ## setter for the inverse
  setInverse <- function(inv) mInv <<- inv
  
  ## return a list of getters and setters
  list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## if the inverse of the matrix is already calculated, return it. If not, calculate it, store it and then return it

cacheSolve <- function(x, ...) {
    ## x is a CacheMatrix
    mInverse <- x$getInverse()
    
    if(is.null(mInverse)) {
        message("Inverting the matrix (calculate)....")
        data <- x$get()
        mInverse <- solve(data, ...)
        
        ## store for future use
        x$setInverse(mInverse)
    }
  
        ## Return a matrix that is the inverse of 'x'
    mInverse
}

## Programming Assignment 2
##
## Contains a special function to hold a cache

## given x as a matrix, provide getters and setters to store x and its inverse
##
makeCacheMatrix <- function(x = matrix()) {
  ## cached inverse -- initialize to NULL
  mInv <- NULL
  
  ## getter for the matrix
  get <- function() x
  
  ## setter for the matrix
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


## Special function to solve the matrix and to cache the result
## 
## if the inverse of the matrix is already calculated, return it. 
## If not, calculate it, store it and then return it

cacheSolve <- function(x, ...) {
  ## Note : x is a CacheMatrix
  
  ## get the inverse; 
  ## this will be NULL the first time and valid everytime afterwards
    mInverse <- x$getInverse()
    
  ## if the result is NULL, this is first time it is invoked  
    if(is.null(mInverse)) {
        message("Inverting the matrix (first time)....")
        
        ## get the original matrix
        data <- x$get()
        
        ## solve for the inverse
        mInverse <- solve(data, ...)
        
        ## store for future use
        x$setInverse(mInverse)
    }
  
  ## Return a matrix that is the inverse of 'x'
    mInverse
}

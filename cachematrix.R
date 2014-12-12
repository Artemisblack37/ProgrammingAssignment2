## This set of functions can take a matrix as its input a result in its inverse.
## To save time the inverse is cached to be accessed later if necessary 

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {#the input of the function is a matrix
  #First set the initial inverse of the matrix to NULL
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  #these next functions are not defined here, but are executed by cacheSolve()
  
  get <- function() {x} #this returns the value of the original matrix
  
  setinv <- function(inverse) {inv <<- inverse}
  #this is called by casheSolve() durring the first execution
  
  getinv <- function() inv #this will return the cached value to cacheSolve() on
    #subsequent accesses
    
  list(set=set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  #Check for null value
  
  if(!is.null(inv)) {
    #prints this message in the console if this function has already been run
    message("getting cached data")
    return(inv)
  }
  
  message("no cached inverse matrix, calculating")
  ##if inv is NULL, get original matrix
  data <- x$get()
  ## and caluculate the inverse
  inv <- solve(data, ...)
  x$setinv(inv)
  ##return the inverse
  inv
}

##This pair of functions allows the user to use a cached value of a matrix inverse 
##rather than recalculating it if the inverse has previously been calculated.  The 
##functions do this by storing, or caching the value the first time it is computed. 

 
## The following funtion "makeCacheMatrix" creates a special "matrix" object
## that can cache the value of its inverse.  It contains functions that:
## 1) set the value of the matrix, 
## 2) get the value of the matrix, 3) set the inverse of the matrix, and
## 4) get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The following function "cacheSolve" calculates the inverse
## of the special "matrix" returned by the above function.  
## However, it first checks to see if the inverse has already been calculated.
## If so it gets the inverse from the cache and skips the calculation.  Otherwise
## it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
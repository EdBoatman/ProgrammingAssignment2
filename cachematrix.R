## Caches the value of an inverse of a matrix for later computation, avoiding 
## the need to repeatedly calculate the inverse

## Creates an object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                 
  set <- function(y) {                      
    x <<- y                           
    inv <<- NULL                        
  }
  get <- function() x                       
  setinverse <- function(solve) inv <<- solve      
  getinverse <- function() inv
  list(set = set, get = get,                
       setinverse = setinverse,
       getinverse = getinverse)
}


## Retrieves the inverse from the cached value stored in the makeCacheMatrix
## environment

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

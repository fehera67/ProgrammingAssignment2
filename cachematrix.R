## Caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    setmatrix <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    
    getmatrix <- function() x
    
    setinversec<- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    
  inv <- x$getinv()

  if (!is.null(inv)) {
   
     message("getting cached data")
     return(inv)
  
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv  
  
}

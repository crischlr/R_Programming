## These functions calculate the inverse of a matrix x wich is supposed not singular.
## Once it is calculated the first time, it is cached so that it can be used again
## if needed (for example, in a loop).
## This is saving time, in case you need to use the inverse of the matrix x repeteadly,
## as the calculation is a very time consuming operation.



## makeCacheMatrix function creates a vector containing functions which will be used
##   - set(): Set values to cached matrices 'x' and 'inv'
##   - get(): Returns the cached matrix 'x'
##   - setinv(): Set the value to cached matrix 'inv'   
##   - getinv(): Returns the value of cached matrix 'inv'

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
  
}


## cacheSolve function returns a matrix that is the inverse of 'x'
## First, it tries to get the cached inverse matrix if exists.
## If the inverse matrix has not been cached yet it is calculated and cached

cacheSolve <- function(x, ...) {

  inv <- x$getinv()
  
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  
  data <- x$get()
  
  # Check if matrix is square
  if (nrow(data) != ncol(data)){
      message("matrix is not square!")
      return(invisible(inv))
  } 
  
  inverse <<- solve(data)
  
  x$setinv(inverse)
  
  inverse
}
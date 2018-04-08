## The goal of this assigment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix is a function. It creates a matrix object that can cache the inverse of an invertible matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() 
  {
    x
  }
  setinv <- function(inverse) 
  {
    inv <<- inverse
  }
  getinv <- function() 
  {
    inv
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cachSolve is a function. It computes the inverse of the special matrix returned by makeCacheMatrix. It retrieves the inverse from the cache if it has been calculated previously.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) 
  {
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

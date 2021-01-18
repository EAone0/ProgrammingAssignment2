## This function creates a special "matrix" object
## that can cache its inverse

## We are working under the assumption that the matrix given is revertible.


makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y){
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) k <<- inverse
  getinverse <- function() k
  list(set = set, get = get, 
       setinverse = setinverse, 
       getInverse = getInverse)
}




## cacheSolve takes the matrix returned by the function makeCacheMatrix
## then computes its inverse, and retrieves it from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  k <- x$getinverse()
  if (!is.null(k)){
      message("getting cached data...")
      return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setinverse(k)
  k
}



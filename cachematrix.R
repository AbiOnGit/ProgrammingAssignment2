## Assignment2: Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse matrix
  inverse <- solve(x)
  setinverse <- function(inverse) m <<- inverse
  
  ## get the value of the inverse matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## Check if the inverse has already been calculated (and the matrix has not changed), 
  ## If so, then cacheSolve should retrieve the inverse from the cache.
  ## and skip the computation of inverse matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Otherwise: If not cached, compute inverse of matrix
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}

## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if the matrix inverse is already cached. If it's cached, it retrieves
## the cached inverse. If it's not cached, it calculates the inverse and stores it in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x["getinverse"]
  if(!is.null(m)) {
    message("getting cached data")
  return(m)
  }
  data <- x["get"]
  m <- solve(data, ...)
  x["setinverse(m)"]
  m
}

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize cache to empty value and assign to 'm'
  m <- NULL
  
  ## initilize matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get matrix value
  get <- function() x
  
  ## invert matrix and assign to m
  setMatrix <- function(solve) m <<- solve
  
  ## read back inverse from cache 'm'
  getInverse <- function() m
  
  ## store all the funcitons in a list
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## cacheSolve checks to see if the matrix inverse is already cached. If it's cached, it retrieves
## the cached inverse. If it's not cached, it calculates the inverse and stores it in cache.
cacheSolve <- function(x, ...) {
  
  ## retrieve inverse from cache if it's already cached
  m <- x$getInverse()
  
  ## if inverse is stored in cache, retrieve it
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if inverse is not in cache use the get function to calculate it
  data <- x$get()
  
  ## solve to calculate inverse
  m <- solve(data, ...)
  
  ## assign matrix to m
  x$setMatrix(m)
  
  ## print out result
  m
}
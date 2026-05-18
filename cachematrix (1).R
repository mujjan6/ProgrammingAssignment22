# makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   # store cached inverse here
  
  # set a new matrix and reset cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # return current matrix
  get <- function() x
  
  # store inverse in cache
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  # get cached inverse
  getinverse <- function() {
    inv
  }
  
  # return functions as a list (acts like an object)
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


# cacheSolve computes the inverse or returns cached value
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # return cached inverse if available
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # compute inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # cache the result
  x$setinverse(inv)
  
  # return inverse
  inv
}


# Create a cacheMatrix object for an invertale matrix.
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  stinv <- function(inverse) cachedInverse <<- inverse
  gtinv <- function() cachedInverse
  list(set = set, get = get,
       setInverse = stinv,
       getInverse = gtinv)
}


# Return the inverse of an cacheMatrix object
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  f_inv <- x$getInverse()
  if(!is.null(f_inv)) {
    message("getting cached data")
    return(f_inv)
  }
  data <- x$get()
  f_inv <- solve(data, ...)
  x$setInverse(f_inv)
  invFunc
}

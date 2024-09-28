
# Create a cacheMatrix object for an invertale matrix.
makeCacheMatrix <- function(x = matrix()) {
  cached_inv <- NULL
  set <- function(y) {
    x <<- y
    cached_inv <<- NULL
  }
  get <- function() x
  stinv <- function(inverse) cached_inv <<- inverse
  gtinv <- function() cached_inv
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

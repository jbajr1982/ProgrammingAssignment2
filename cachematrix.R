
## Create a cacheMatrix object for an invertale matrix.

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


## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}

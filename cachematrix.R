makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {

  invMatrix <- x$getInverse()
  if (!is.null(invMatrix)) {
    message("wait...")
    return(invMatrix)
  }
  mat <- x$get()
  invMatrix <- solve(mat, ...)
  x$setInverse(invMatrix)
  invMatrix
}
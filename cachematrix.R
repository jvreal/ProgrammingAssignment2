## To increase performance these helper functions provide way to use
## a special matrix that can cache its inverse.  This can be utilized vs
## do normal matrix operations and having to recalculate inverse everytime

## Creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    message("set")
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(solve) inv <<- solve
  getinvmatrix <- function() inv
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## Computes inverse of special matrix returned by makeCacheMatrix.  If inverse has already
## been calculated then inverse is retrieved from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvmatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinvmatrix(inv)
  message("return inverse")
  inv
}

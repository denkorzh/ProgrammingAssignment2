## As a result, makeCacheMatrix creates a special object 
## that contains a cached matrix, its cached inverse
## and gives 4 functions to deal with the matrices.
## cacheSolve finally returns the inverse of the given matrix
## and provides the inverse to be cached.

## This function creates a special "matrix" 
## which is a list containing functions to
## - set the value ('x') of the matrix ('set')
## - get the value ('x') of the matrix ('get')
## - set the value ('inv') of the inverse of the matrix ('setinv')
## - get the value ('inv') of the inverse of the matrix ('getinv')

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## If the inverse of the argument was computed and cached before
## the function shows the message and returns the inverse of 'x'.
## If there's no value cached it calculates the inverse, puts it
## in the cache and returns the inverse asa the result

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
}

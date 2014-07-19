## This pair of functions caches the inverse of a matrix.  So when the inverse of
## matrix has already been computed, the value will be retrieve from the cache, 
## hence save time.

## x is the matrix for which the inverse we want to store in cache
##
## Return a list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## x is the matrix that needs to be inverted
##
## The function first check if the answer is available in cache
## If yes, it will return that value
## If not, it will compute the inverse of the matrix
##
## Returns a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
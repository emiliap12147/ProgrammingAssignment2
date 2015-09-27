## makeCacheMatrix creates an object ("matrix") that can cache its inverse
## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix


## makeCacheMatrix takes one argument x (a matrix)
## makeCacheMatrix is a function, which stores four functions (set, get, setinverse and getinverse)
## it returns a list of all 4 functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes the object in which makeCacheMatrix is stored as input
## it verifies if the value i stored in makeCacheMatrix exists and is not null
## if i exists in memory, a message and the value i are returned
## if not, the matrix stored in makeCacheMatrix is used to calculate its inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

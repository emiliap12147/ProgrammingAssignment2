## makeCacheMatrix creates an object ("matrix") that can cache its inverse
## set changes the matrix stored in main function
## get returns the matrix x stored in main function
## setinverse stores the value of the input in a variable i into makeCacheMatrix
## getinverse returns the stored input stored in the variable i
## all four functions are stored in a list

## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix
## cacheSolve verifies if the value i stored in makeCacheMatrix exists and is not null
## if i exists in memory, a message and the value i are returned
## if not:
## 1) data gets the matrix stored in makeCacheMatrix
## 2) i calculates the inverse of the matrix
## 3) x$setinverse(i) stores the inverse in the object assigned with makeCacheMatrix
## 4) the inverse of the matrix is printed


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

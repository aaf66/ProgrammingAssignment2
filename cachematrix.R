# assignment
# 1. This function creates a special "matrix" object that can cache its inverse.
# use solve() to calculate the inverse of the matrix
# matrix to test
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
A

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) mat_inv <<- solve
  getinv <- function() mat_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
# test the function on the test matrix A
test <- makeCacheMatrix(A)

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  }  

# test the function
cacheSolve(test)
solve(A)



#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
# ************SAMPLE RUNNING************
#x <- matrix(1:7, nrow=4, ncol=4)
#> x
#[,1] [,2] [,3] [,4]
#[1,]    1    5    2    6
#[2,]    2    6    3    7
#[3,]    3    7    4    1
#[4,]    4    1    5    2

#inv = makeCacheMatrix(x)
#inv$get()
#cacheSolve(inv)

#           [,1]       [,2]       [,3]       [,4]
#[1,] -4.0000000  3.5714286 -0.1428571 -0.4285714
#[2,] -0.1428571  0.1428571  0.1428571 -0.1428571
#[3,]  3.2857143 -3.0000000  0.1428571  0.5714286
#[4,] -0.1428571  0.2857143 -0.1428571  0.0000000

#cacheSolve(inv)

#getting cached data
#[,1]       [,2]       [,3]       [,4]
#[1,] -4.0000000  3.5714286 -0.1428571 -0.4285714
#[2,] -0.1428571  0.1428571  0.1428571 -0.1428571
#[3,]  3.2857143 -3.0000000  0.1428571  0.5714286
#[4,] -0.1428571  0.2857143 -0.1428571  0.0000000

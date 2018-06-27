## Matrix inversion is usually a costly computation and there may be 
##some benefit to caching the inverse of a matrix rather than compute 
##it repeatedly (there are also alternatives to matrix inversion that 
##we will not discuss here).  Below assignment is to write a pair of functions
##that cache the inverse of a matrix.

##makeCacheMatrix: This function creates a special "matrix" object that 
##can cache its inverse
# Steps: creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  val <- NULL
  set <- function(y) {
    x <<- y
    val <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) val <<- inverse
  getinverse <- function() val
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  val <- x$getinverse()
  if(!is.null(val)) {
    message("fetching cached data...")
    return(val)
  }
  data <- x$get()
  val <- solve(data, ...)
  x$setinverse(val)
  val
}

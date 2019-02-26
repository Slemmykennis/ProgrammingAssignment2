## This function conputes the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      invk <- NULL
      set <- function(y) {
            x <<- y
            invk <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) invk <<- inverse
      getinverse <- function() invk
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated(and the matrix has not changed)
## the cachesolve retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invk <- x$getinverse()
      if(!is.null(invk))  {
            message("getting cached data")
            return(invk)
      }
      mat <- x$get()
      invk <- solve(mat, ...)
      x$setinverse(invk)
      invk
}


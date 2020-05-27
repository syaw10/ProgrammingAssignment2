## This is my submission for Assignment #2 of the Data Science course offered by
## Johns Hopkins University on Coursera.  The function creates a matrix, x, that can 
## cache its own inverse.  The function returns an error if det(x)=0.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
      }
      

## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

 cachesolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
      }

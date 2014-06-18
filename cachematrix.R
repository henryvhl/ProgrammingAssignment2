## CACHEMATRIX.R
## A pair of functions that cache the inverse of a matrix

## MAKECACHEMATRIX(x)
## This function creates a special "matrix" object that can cache 
## the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() { x }
    setInverse <- function(inverse) { inv <<- inverse }
    getInverse <- function() { inv }
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## CACHESOLVE(x, ...)
## This function computes the inverse of the special "matrix" x 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setInverse(inv)
    inv
}

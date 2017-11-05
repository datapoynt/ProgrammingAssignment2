## These functions use caching to avoid the costly computation of repeatedly
## doing the matrix inversion

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinv <- function(inv) v <<- inv
        getinv <- function() v
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        v <- x$getinv()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinv(v)
        v        
}

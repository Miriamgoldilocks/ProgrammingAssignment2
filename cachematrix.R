## A set of funtions that enable the caching of a matrix inverse.
## Matrix inversion often is a costly computation and therefore we can benefit 
## from having the option of caching the inverse of a matrix rather than having
## to compute it repeatedly. 
## Below we have a pair of functions that can be used to create a matrix like 
## object that additionaly caches its inverse and only calculate the inverse if
## it wasn't calculated before.

## The function makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(invers) inv <<- invers
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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


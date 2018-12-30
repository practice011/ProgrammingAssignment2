## This pair of functions cab cache the inverse of a matrix. This can be useful because 
## matrix inversion is usually a costly computation. Here, we first check if it first checks to see if
## the inverse of a particular matrix has already been calculated. If so, we get the mean from the cache and skip the computation.

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

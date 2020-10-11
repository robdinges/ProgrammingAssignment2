## MakeCacheMatrix returns an object with all getters and setters for the 
## inversed matrix. This returned object can be used as an input parameter by
## CacheSolve to check for a cached version of the inversed matrix and return 
## that or to update the cache 

## Create and return the list object including the setters and getters.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solved) m <<- solved
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Check if the current cached inversed matrix is valid (not NULL) and otherwise
## calculate it and cache it again, using the functions that were passed with
## the input object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m        
}


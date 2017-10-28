## Functions to calculate the inverse of a matrix and cache it.

## First function creates a list that stores (caches) a matrix and its inverse
## Constructing functions allows to set the matrix, set the inverse,
## retrieve the matrix, retrieve its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

## Second function will take a list in the same format as the output of first one
## It will retrieve the value of the inverse in the input list. If it already exists,
## the cached value is returned. Otherwise, the inverse is calculated and stored
## in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

## The function makeCacheMatrix creates a "matrix" object 
## that can cache its inverse. The function cacheSolve returns
## the inverse of the "matrix" object created by makeCacheMatrix.
## If the inverse has already been computed, it returns the 
## retrieved value (saving time); otherwise it computes the inverse
## and stores the value for future retrieval.

## The function below initializes the "matrix" object and defines 
## the four functions that will set the matrix, retrieve the matrix, 
## set the inverse, and retrieve the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of a matrix and caches it. 
## If the inverse has already been computed, it returns 
## the cached inverse. 

cacheSolve <- function(x, ...) {
        
        ## First, look to see if the inverse has already been computed
        ## and cached. If so, return the inverse without computing it again.
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Otherwise, compute the inverse, cache it, and return.
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

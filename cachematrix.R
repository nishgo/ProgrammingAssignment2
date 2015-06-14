## Description: Provides functions to compute the inverse of a matrix, and 
## cache both the original and inverse matrices for faster reuse.

## makeCacheMatrix allows for caching and retreival of a matrix (normal_m)
## and its inverse (inverse_m). It provides four subfunctions:
## set       caches new matrix (new_m) in matrix (normal_m)
## get       returns cached matrix (normal_m)
## setInv    caches new inverse matrix (new_i) in inverse matrix (inverse_m)
## getInv    returns cached inverse matrix (inverse_m)
makeCacheMatrix <- function(normal_m = matrix()) {
    # Define a null variable named 'inverse" to hold the inverse matrix
    inverse_m <- NULL
    
    # Define the subfunctions
    set <- function(new_m) {
        normal_m  <<- new_m
        inverse_m <<- NULL
    }
    get <- function() normal_m
    setInv <- function(new_i) {
        inverse_m <<- new_i
    }
    getInv <- function() inverse_m
    
    # Create a list of the subfunctions so that they may be accessed
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve takes a makeCacheMatrix function list argument x and calls the 
## getInv function from list x to retreive the cached inverse matrix. If the
## inverse matrix is not already cached, cacheSolve computes the inverse matrix,
## then calls the setInv function from list x to cache the inverse matrix for
## future lookups.
cacheSolve <- function(x, ...) {
    # Retrieve the cached matrix
    cached <- x$getInv()
    if (!is.null(cached)) {
        message("getting cached data")
        return(cached)
    }
    
    # No cached matrix; get matrix to invert
    normal_m <- x$get()
    
    # Get the inverse matrix
    inverse_m <- solve(normal_m, ...)
    
    # Cache the inverse matrix
    x$setInv(inverse_m)
    
    # Return and print the inverse matrix
    return(inverse_m)
}

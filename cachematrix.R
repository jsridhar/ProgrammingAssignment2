# Creates a special matrix object that can cache its inverse.
#
# makeCacheMatrix creates a special matrix, which is really a list of containing a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix
# 4. Get the value of the inverse of the matrix
#
makeCacheMatrix <- function(x = matrix()) {
    
    invMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv) invMatrix <<- inv
    
    getInverse <- function() invMatrix
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


# Computes the inverse of the special matrix returned by makeCacheMatrix function. If the inverse has already been calculated 
# (and the matrix has not changed), then the cacheSolve returns the inverse from cache.
# 
# @ param x Input square (invertible) matrix to be inverted
# @ return matrix Output inverted matrix
# 
cacheSolve <- function(x, ...) {
    
    invMatrix <- x$getInverse()
    
    if (is.null(invMatrix)) {

        invMatrix <- solve(x$get())
        x$setInverse(invMatrix)
        
    } else {

        message("Getting cached data...")
        
    }
    
    invMatrix
}

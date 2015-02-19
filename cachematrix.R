## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can cache its inverse
## It ensures that if a new matrix is set, the inverse is reset
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL ##Instance variable for inverse
    
    set <- function(y) {
        x <<- y ##set matrix as a variable out of scope
        i <<- NULL ##set inverse to NULL (forces re-calculation)
    }
    get <- function() x ##getter method for matrix variable
    
    ##set inverse to a variable out of scope to cache it
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i ##getter to return inverse
    
    ##Return functions
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## This function solves the inverse if there is no cash
## or it returns the cache if one exists.
cacheSolve <- function(x, ...) {

    #n.b.  Assignment assume all matrices supplied are invertible
    # therefore no checks performed
    
    cached <- x$getInverse() ##get any cache present
    if ( !is.null(cached) ) {
        message("getting cached data")
        return(cached)
    }
    
    data <- x$get() ##Get the matrix created in makeCacheMatrix
    inverse <- solve(data, ...) ##solve and create inverse
    x$setInverse(inverse)  ##set the inverse to cache the result
    inverse ## Return a matrix that is the inverse of 'x'
}
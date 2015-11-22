## Put comments here that give an overall description of what your
## functions do

# This function create a simple object with matrix and inverse fields

makeCacheMatrix <- function(x = matrix()) {
    matrix <- x
    inverse <- NULL
    
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    get <- function() matrix
    setinverse <- function(y) inverse <<- y
    getinverse <- function() inverse
    
    list(set = set, 
         get = get,
         getinverse = getinverse,
         setinverse = setinverse)
}


## Write a short comment describing this function

# This function check if a cache inverse already exists.
# If it is the case the function return the cache result,
# Otherwise the function compute the inverse of the matrix and cache the result.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

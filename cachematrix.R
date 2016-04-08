# The follwing are a pair of functions that cache the inverse of a matrix.

# Fuction One is "makeCacheMatrix" 
# This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# Function Two is "cacheSolve"
# This function computes the inverse of the special matrix returned by 
# makeCacheMatrix above. If the inverse has already been calculated, then
# then the cacheSolve should retrieve the inverse from the cache.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

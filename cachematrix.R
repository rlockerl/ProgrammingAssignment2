## 
## This function creates a special "matrix" which is really 4 functions to allow caching of the inverse of a matrix.
## 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    # store the base matrix and set the inverse to null
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    # return the stored matrix
    get <- function() x

    # cache the inverse of the base matrix
    setinverse <- function(inverse) i <<- inverse

    # retrieve the cached inverse of the base matrix
    getinverse <- function() i

    # return the four functions created above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##
## Solve for the inverse of a special matrix, using the cached value when available.
##
cacheSolve <- function(x, ...) {
    # fetch the inverse of the special matrix
    i <- x$getinverse()

    # return the cached inverse (if available)
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }

    # otherwise, retrieve the base matrix
    data <- x$get()

    # compute the inverse
    i <- solve(data, ...)

    # store it in the cache
    x$setinverse(i)

    # return it
    i
}

# cachematrix.R

## These functions create a "special" matrix object that allows the inverse to be cached and returned

## Create the "special" matrix. Returns a list of methods such as get, set, getinverse, and setinverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Returns the inverse of the matrix. Does so by accessing the cached inverse or computing.
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    matrix_data <- x$get()
    i <- solve(matrix_data)
    x$setinverse(i)
    i
}


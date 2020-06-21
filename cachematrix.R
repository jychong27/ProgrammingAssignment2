## The 'makeCacheMatrix' allow us to create special
## matrix and stored in it

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The "cacheSolve" function is used to return inverse matrix
## back to the function above

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setInverse(i)
    i
}
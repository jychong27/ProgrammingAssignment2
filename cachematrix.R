## It's always troublesome to compute inverse matrix
## It does contribute benefits by caching the inverse matrix
## It's better that we cache the inverse matrix rather than
## we store and compute each matrix repeat and repeat

## The "makeCacheMatrix" helps to create a special
## matrix so that it can cache its inverse value

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

## The "cacheSolve" function is used to compute 
## inverse matrix of the special matrix obtained from
## function above. 
## If it had been calculated, therefore there will be no
## changes, and the it would return the inverse to the cache

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
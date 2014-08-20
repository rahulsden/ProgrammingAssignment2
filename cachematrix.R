
## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    xinverse <- NULL
    set <- function(y) {
        x <<- y
        xinverse <<- NULL
    }
    get <- function() x
    setinverse <- function(minverse) xinverse <<- minverse
    getinverse <- function() xinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    minverse <- x$getinverse()
    if (!is.null(minverse)) {
        message("returning cached data")
        return(minverse)
    }
    m <- x$get()
    minverse <- solve(m)
    x$setinverse(minverse)
    minverse
}

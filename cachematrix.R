## Build a matrix which inverse can be cached
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


## Get the inverse of a matrix
## a) from the cache if it has been previously calculated and cached
## b) from calculations if its the first time
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}

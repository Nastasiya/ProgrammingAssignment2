## These two functions can be used to cache the inverse of a matrix

## This function creates a "matrix" object

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the inverse of matrix 'x' (either by calculating it 
## or by taking the cached value if such exists)

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
        message("Getting cached inverse...")
        return(i)
    }
    matr <- x$get()
    i <- solve(matr)
    x$setInverse(i) 
    i
}

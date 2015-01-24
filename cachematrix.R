## makeCacheMatrix creates a matrix whose inverse can be computed 
## only if it has not been already calculated
## cacheSolve matrix computes the inverse of the matrix created 
## using the above function

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get=get, 
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of the special matrix return by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

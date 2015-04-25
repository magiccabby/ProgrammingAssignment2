## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## init matrix inverse
    m <- NULL
    ## set the value of the matrix and init matrix inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get the value of the matrix
    get <- function() x
    ## set matrix inverse
    setInverse <- function(inverse) m <<- inverse
    ## get matrix inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## check whether the cached inverse is empty or not
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## compute the matrix inverse
    data <- x$get()
    m <- solve(data, ...)
    ## cache the computed inverse 
    x$setInverse(m)
    m
}

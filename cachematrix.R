## cachematrix.R provides two functions makeCacheMatrix() 
## and cacheSolve(). A cache is created of a matrix in
## the parent environment allowing information to be 
## retrived without the need to recalulate the inverse
## Useage:- cachedM     <- makeCacheMatrix(matrixToCache)
##          inversion   <- cacheSolve(cachedM)



## makeCacaheMatrix takes a invertible matrix and 
## stores in the parent environment in the variable 'x'.
## Returns a list of created functions to calculate and 
## set the inverse of matrix and to get it from cache.
## Prints a message when the cache is being used

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list( set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix which
## was supplied to the makeCacheMatrix; See Use above.
## Also prints a message when the cache is being used.
## On initial call only the newly inverted matrix is
## returned and at the same time the cache is updated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m) 
    }

    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m
}

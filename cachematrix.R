## Create a special matrix object, that stores the matrix as well as its inverse
## The inverse is computed and stored in the cache the first time, and on
## subsequent calls, the inverse is returned from the cache

## Takes in a matrix and creates a special matrix object
## Returns a list of setters & getters to the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## 'cached' version of solve(x)
## Takes in a list of setters & getters to a matrix as created by
## makeCacheMatrix(x), and returns the inverse of the matrix,
## from the cache (if avbl), else computes & returns the inverse,
## as well as stores it in the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

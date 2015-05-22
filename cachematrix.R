## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## i: cached inverse of matrix 
## getinv: get cached inverse
## setinv: set cached inverse
## set: set matrix to obj
## get: get matrix from obj
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setinv<- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## if inverse exists in cache, get cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

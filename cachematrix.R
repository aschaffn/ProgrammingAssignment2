## The following functions are used to avoid unncecessary 
## computation the inverse of a matrix. 

## makeCacheMatrix is a utility function. In its environment
## there are four functions (set, get, setinv, getinv). set
## is used to reset the matrix and set the variable i to NULL 
## so that the inverse will be computed later. 
## get is used to call the matrix. setinv is used to set the 
## stored value of the inverse. getinv is used to get the value of
## the stored inverse. These values all "live" in the makeCacheMatrix
## environment.

## There is a single argument, x, a matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
     set <- function(y) {
         x <<- y
         i <<- NULL
     }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, 
        get = get,
        setinv = setinv,
        getinv = getinv)
}

## cacheSolve is used to compute the inverse of a matrix using the 
## helper function makeCacheMatrix. It first checks to see if 
## the inverse has been computed earlier. If so, this stored 
## inverse is reported. If not, the inverse is computed and 
## stored within the makeCacheMatrix environment.

## There is a single required argument, a list, x, which are the 
## results of makeCacheMatrix.

## Example:
## xmpl = makeCacheMatrix(matrix(1:4, 2))
## cacheSolve(xmpl)
## On the first run of cacheSolve, the inverse is computed and returned.
## 
## Running it a second time...
## cacheSolve(xmpl)
## the inverse is not computed (since it was on the first call), 
## the precomputed value is returned.


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

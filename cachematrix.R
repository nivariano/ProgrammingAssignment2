## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## This code implements a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                     ## Initialize local R object that will contain cached inverse matrix
    setmat <- function(y) {         ## Set matrix value which cacheSolve function will use for its calculations
        x <<- y                     ## Take matrix argument and set it as new matrix value in the parent environment
        inv <<- NULL                ## Reset the cached inverse matrix to NULL in the parent environment
    }
    getmat <- function() {          ## Get matrix current value
        x                           ## Return current matrix
    }
    setinv <- function(inverse) {   ## Set the cached inverse matrix value
        inv <<- inverse             ## Take calculated inverse matrix and set it as new value for cached inverse matrix in the parent environment
    }
    getinv <- function() {          ## Get inverse matrix current value
        inv                         ## Return cached inverse matrix
    }
    list(setmat = setmat,           ## Return list of the functions defined above
         getmat = getmat,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()               ## Set inverse matrix value to the current cache value
    if(!is.null(inv)) {             ## If current cache value is not NULL then return this value and exit function
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$getmat()              ## Otherwise, get current matrix value into local variable "data"
    inv <- solve(data, ...)         ## and set inverse matrix value to the calculated inverse matrix of "data"
    x$setinv(inv)                   ## Cache the calculated inverse matrix
    inv                             ## Return the calculated inverse matrix
}

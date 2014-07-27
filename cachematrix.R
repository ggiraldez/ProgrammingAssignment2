## These functions are used to compute the inverse of a matrix and cache it for
## repeated uses (aka. memoization). To use the functions, you must:
##
## - wrap your matrix in an R object using makeCacheMatrix()
## - use cacheSolve() on the object to retrieve the inverse of the matrix
##
## Example:
## > x <- makeCacheMatrix(matrix(1:4, 2, 2))
## > inv.x <- cacheSolve(x)
##
## successive calls to cacheSolve(x) will yield the cached result


## makeCacheMatrix(x) creates an R object capable of holding a matrix and cache
## its inverse. You can access the original matrix by calling the get()
## function of the object, like:
##
## x <- makeCacheMatrix(matrix(1:4, 2, 2))
## y <- x$get()
##
## y now contains matrix(1:4,2,2)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL           # holds the cached inverse of x

    set <- function(y) {
        x <<- y
        inv <<- NULL      # clear the cached inverse when resetting the matrix value
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv

    # return the object with its "methods"
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Use cacheSolve(x) to retrieve the inverse of a matrix. x must be an R object
## returned by the makeCacheMatrix() function above. This function will try to use
## the cached value of the inverse held by the cacheMatrix object and revert to
## calling solve() if the value is not available, saving the result for future
## uses.
##
## > x <- makeCacheMatrix(matrix(1:4, 2, 2))
## > inv.x <- cacheSolve(x)
## > cacheSolve(x) %*% x$get()
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1

cacheSolve <- function(x, ...) {
    inv <- x$getinv()         # get cached value first
    if (!is.null(inv)) {
        # return cached value if it's valid
        message("getting cached data")
        return(inv)
    }

    # revert to calculating the inverse
    data <- x$get()
    inv <- solve(data, ...)

    # but save the result for future uses
    x$setinv(inv)
    inv
}


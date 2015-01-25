## These functions combined will cache the inverse of the matrix and 
## return the cached value if present or compute and return if not.


## makeCacheMatrix function is used to create and retrieve matrix objects
## it can also set and retrieve the inverse of the matrix object

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function checks the matrix object created using makeCacheMatrix
## for the matrix inverse. If matrix inverse is cached it is returned else
## the inverse is computed and then returned

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

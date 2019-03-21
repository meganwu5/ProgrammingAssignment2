## Below are a pair of functions which are used to create a special object that stores a matrix and caches its inverse.
## If the matrix inverse has already been calculated, it will instead find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
}
    get <- function() x
    setinverse<- function(inverse) inv <<-inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by "makeCacheMatrix" above.
## If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv)
        return(inv)
    }
}

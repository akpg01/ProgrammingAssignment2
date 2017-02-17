## because matrix inversion is usually a costly computation. There may be a
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The two functions below aid in caching the previously computed matrix inversion
## and computes the inverse matrix.

## creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


# finds the inverse of the special matrix, but prior
# to doing so, checks whether the matrix inverse is already cached.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

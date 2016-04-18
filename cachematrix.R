
## Caching the Inverse of a Matrix



## makeCacheMatrix: This function creates a special "matrix"
## object that can cache its inverse. 

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


## cacheSolve: This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.Put comments here that give an overall description of what your
cacheSolve <- function(x, ...) {
    ii <- x$getinv()
    if(!is.null(ii))
    {
        return (ii)
    }
    data <-x$get()
    ii <- solve(data,...)
    x$setinv(ii)
    ii
    
}

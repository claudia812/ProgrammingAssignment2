## Caching the Inverse of a Matrix

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(MATRIX = matrix()) {IMATRIX <- NULL
        set <- function(y) {
                MATRIX <<- y
                IMATRIX <<- NULL
        }
        get <- function() MATRIX
        setinverse <- function(solve) IMATRIX <<- solve
        getinverse <- function() IMATRIX
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(MATRIX, ...) {
        IMATRIX <- MATRIX$getinverse()
        if(!is.null(IMATRIX)) {
                message("getting cached data")
                return(IMATRIX)
        }
        data <- MATRIX$get()
        IMATRIX <- solve(data, ...)
        MATRIX$setinverse(IMATRIX)
        IMATRIX
}

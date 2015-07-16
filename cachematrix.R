## Caching the Inverse of a Matrix

## makeCacheMatrix function creates a special matrix
## setMatrix:           set the value of the matrix
## getMatrix:           get the value of the matrix
## cacheInverse:        set the value of the inverse matrix
## getInverse:          et the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() {
                x
        }
        cacheInverse <- function(solve) {
                m <<- solve
        }
        getInverse <- function() {
                m
        }
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## cacheSolve function inverses of the matrix created with the function makeCacheMatrix
## It first checks to see if the matrix has already been inversed
## If so, it gets the inversed matrix from the cache and skips the computation
## Otherwise, it inverses the matrix and sets the value of the inversed matrix in the cache via the cacheInverse function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        inve <- x$getMatrix()
        m <- solve(inve, ...)
        x$cacheInverse(m)
        m
}

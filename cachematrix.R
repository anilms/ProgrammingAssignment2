## Functions to speed up Matrix inverse operation by Caching
## When inverse of same matrix is being called multiple times,
## the value is returned from the cache instead of recalculation

## Function to create a special copy of the matrix
## This stores the matrix in separate environment using "<<"
## Also it has functions to set and get the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) mInv <<- Inv
        getInv <- function() mInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Returns the inverse of the matrix
## Speeds up the process by returning value from cache
## If inv was not calculated before, it uses 'solve' to get inverse
## The value in cache is also updated for next time
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <- x$getInv()
        if(!is.null(mInv)) {
                message("getting cached data")
                return(mInv)
        }
        data <- x$get()
        mInv <- solve(data, ...)
        x$setInv(mInv)
        mInv
}

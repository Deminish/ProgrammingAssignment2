## Title: Caching the Inverse of a Matrix
## Description: Solve the inverse of matrices, cachine results to save
## on computation

## Create a special matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Compute the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated then retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setInverse(m)
        m
}

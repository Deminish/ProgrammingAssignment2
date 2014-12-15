## Title: Caching the Inverse of a Matrix
## Description: Solve the inverse of matrices, cachine results to save
## on computation

## Create a special matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        
        #create the null vector m
        m <- NULL
        
        set <- function(y){
                #cache the values so they don't need to be recalculated
                x <<- y
                m <<- NULL
        }
        #set get to x
        get <- function() x
        
        #solve the inverse of the matrix and cache the value the m
        setInverse <- function(solve) m <<- solve
        
        #retrieve the value for the solved cached matrix m
        getInverse <- function() m
        
        #the output of this function, assigning values to each item in the list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Compute the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated then retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #sets m to the cached value for the inverse of the matrix
        m <- x$getInverse()
        
        #if m has been assigned a value, print a message and return the value
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        # if the inverse was not calculated, calculate the inverse
        data <- x$get()
        m <- solve(data, ...)
        
        #set the inverse and return the value
        x$setInverse(m)
        m
}

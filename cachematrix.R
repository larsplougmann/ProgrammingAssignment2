## Two functions that work together to calculate the inverse of a matrix,
## caching the result.

## The makeCacheMatrix takes a matrix and adds the facility for storing the
## value of the inverse matrix. Functions are defined to set and get the
## cached value.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL          ##  m will be the inverse and it's reset to NULL every 
                           ##  time makeCacheMatrix is called. Setting m to null
                           ##  allows us to use the null value as a signal
                           ##  that the matrix has been created but not solved yet.
        
        ##  These following functions are defined but not run when makeCacheMatrix is called.
        ##  Instead, they will be used by cacheSolve()
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() { x } ## Return the original value of the matrix.
        
        ## Functions to set the cached value and return (get) it.
        ## These functions are called fro cacheSolve.
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        ## List out the internal functions so the calling function can
        ## access these methods:
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve takes as input an object created by makeCacheMatrix.
## The function checks if the inverse has already been cached.
## If the cache is different from NULL then fetch from cache.
## If the cache is NULL then calculate the inverse and set the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()                     ## Call x's getinverse function
        if(!is.null(m)) {                       ## If m is not NULL then the inverse
                                                ## has already been calculated
                                                ## and cached
                message("getting cached data")
                return(m)                       ## stop execution and return the
                                                ## cached value
        }
        data <- x$get()                         ## this bit is executed if nothing
        m <- solve(data, ...)                   ## was cashed. It solves for the 
        x$setinverse(m)                         ## inverse and uses the method to
        m                                       ## store the value
        
}

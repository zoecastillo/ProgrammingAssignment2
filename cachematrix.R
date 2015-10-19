## The function creates an object m which is initially set to null
## Several functions are defined:
## set is a function that stores the given parameter y into the outer 
## environment parameter x and sets the cached inverse m to null
## get is a function that returns matrix x
## setinverse is a function that stores the result of solve to the m using
## the super-assignment operator (not restricted to enclosing environment)
## (the actual caching operation)
## getinverse returns the matrix m (the cache)
## the return value of the function is a list of those four functions

## This function creates a "matrix" object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This functions calculates the inverse of a given matrix x
## It checks if the value has been cached before or if not
## it performs the actual calculation and stores the result
## by calling the setinverse element of makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

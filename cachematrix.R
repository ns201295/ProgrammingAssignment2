## The function can calculate the inverse of the matrix and then 
## save it in the cache. The function makeCacheMatrix is the first
## step to create a matrix and the function cacheSolve is to solve  
## the matrix, get the inversion and save it in the cache

## This function makeCacheMatrix is the first step to create a matrix
## and no calculation has been run
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
        
## This function is to solve the matrix, get the inversion and 
## save it in the cache for the future use
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

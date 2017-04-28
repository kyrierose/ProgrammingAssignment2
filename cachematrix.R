## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## getter/setter for matrix
    get <- function() x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## getter/setter for matrix inverse
    setinverse <- function() inverse
    getinverse <- function(inv_para) inverse <- inv_para
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    # return cached matrix inverse if it's been already computed
    if (!is.null(inverse)) {
        message("inverse is cached")
        return(inverse)
    }
    
    # compute inverse of matrix 
    m <- x$get()
    inverse <- solve(m, ...)
    
    # cache inverse
    x$setinverse(inverse)
    
    # return inverse of matrix
    return(inverse)
}

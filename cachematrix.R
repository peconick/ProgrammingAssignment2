## this function initialize the "special" matrix and define get 
## and set functions for both the input matrix and it's inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    #the solution matrix must always be NULL for a new object
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        #if the matrix has changed the solution is no longer valid
        inverseMatrix <<- NULL
    }
    #retrieves the matrix
    get <- function() x
    #set the solution matrix for the desired value
    setinverse <- function(inv) inverseMatrix <<- inv
    #retrieve the solution matrix
    getinverse <- function() inverseMatrix
    
    #returns the aviable functions fot thes class
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function retrieves the value of the cached solution matrix or calculates it
## and save the cache to the special matrix when no cache is avaiable 
## and returns the solution matrix

cacheSolve <- function(x, ...) {
    ## Check it the solution is cached and returns it if avaiable
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If the inverse matrix is not cached, calculates the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    ## Saves calculated inverse to cache of the special matrix
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}

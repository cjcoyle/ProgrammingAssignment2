## The following two functions allows users to:
##    1.) create a matrix
##    2.) calculate its inverse and store the resulting matrix

## makeCacheMatrix creates a special "matrix" as a list of four functions:
##    1.) set: sets the value of the matrix
##    2.) get: gets the current matrix
##    3.) setInverse: sets the value of the inverse matrix
##    4.) getInverse: gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    set <- function(y) {
        
        x <<- y
        inverse <<- NULL
        
    }
    
    get <- function() {
        
        x
        
    }
    
    setInverse <- function(inv) {
        
        inverse <<- inv
        
    }
    
    getInverse <- function() {
        
        inverse
        
    }
    
    list (set = set, 
          get = get, 
          setInverse = setInverse, 
          getInverse = getInverse)
    
}


## cacheSolve computes the inverse of the given matrix, if it hasn't already been computed.
## If the inverse has already been calculated, a cached version of the inverse is outputted.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        
        message("getting cached data")
        
        return(inverse)
        
    }
    
    current_matrix <- x$get()
    
    inv <- solve(current_matrix)
    
    x$setInverse(inv)
    
    return(inv)
    
}

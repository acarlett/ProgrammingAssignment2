## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    ## begins by setting the inv_x to NULL as a placeholder for a future value
    inv_x <- NULL 
    
    ## defines a function to set the vector, x, to a new vector, y, and resets the 
    ## inv_x, m, to NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    
    ##returns the vector, x (it's the same as: get <- function()  {x})
    get <- function() x
    
    ##sets the inverse, inv_x, to inverse
    setinverse <- function(inverse) inv_x <<- inverse
    
    ##returns the inverse, inv_x
    getinverse <- function() inv_x
    
    ##returns the 'special vector' containing all of the functions just defined.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## calculates the inverse of the special "vector"
    inv_x <- x$getinverse()
    ## first checks to see if the inverse has already been calculated
    if(!is.null(inv_x)){
        ## gets the inv_x from the cache
        message("getting cached data")
        return (inv_x)
    }
    
    ## calculates the inverse of the data
    
    ## assign to data the vector x
    data <- x$get()
    
    ## Calculate the inverse and assign it to inv_x
    inv_x <- solve(data,...)
    
    ## sets the value of the inverse in the cache via the setinverse function
    ## Store the inverse "inv_x" under the parameters "vector x".
    x$setinverse(inv_x)
}

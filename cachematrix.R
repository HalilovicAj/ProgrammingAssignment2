## I have defined two functions: 

## a) one for storing a matrix x and its inverse matrix (in case it was already calculated 
## by the second function) in a new object mat, which actually consists of 4 functions, and

## b) the other for calculating the inverse matrix of x, unless it is already stored in mat.

## After saving the functions, you can test them using the following example:
    
##     x <- matrix(c(2,2,3,2),nrow=2)
##     mat <- makeCacheMatrix(x)
##     cacheSolve(mat)
    
## and then again:

##     cacheSolve(mat)
    

## Below is the first function called makeCacheMatrix, which stores a matrix x together 
## with some additional functionalities, such as the function get() which displays the 
## matrix itself, or set() which sets a new value for the matrix (and hence also has to 
## set the inverse to NULL in case one had been calculated for the old matrix).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function returns the cached inverse of mat (actually of x) if one had been
## previously stored in mat, otherwise calculates it and calls the first function to cache 
## the inverse for later use (by calling z$setinverse(inv)).

cacheSolve <- function(z, ...) {
    inv <- z$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- z$get()
    inv <- solve(data, ...)
    z$setinverse(inv)
    inv
    
}

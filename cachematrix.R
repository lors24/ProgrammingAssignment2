## These  functions create a special object that stores a matrix 
## and caches its inverse rather than requiring recalculation every time. 

## makeCacheMatrix creates a special matrix object that can cache its inverse.
## It is a list containing a function to set and get the value of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # set initial solution to NULL
    I<-NULL
    
    # one can chance the original matrix by using the set() function
    # x now takes the new y value and the solution is set to NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    
    # get() returns the stored matrix
    get <- function() x
    
    # setinverse() stores the inverse 
    setinverse <- function(sol) I <<-sol
    
    # getinverse() returns the stored value of I
    getinverse <- function() I
    
    #returns the list with the functions previously created
    list(set = set, get = get,
         setinverse=setinverse,
         getinverse = getinverse)
}

## The following function calculates de inverse of the special matrix
## created with the function makeCacheMatrix.  
## If the inverse has already been calculated, it gets the inverse from the cache. 
## Otherwise, it calculates the inverse of the matrix and sets the value of 
## the inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
    
    # Retrieves the stored value of the inverse
    I <- x$getinverse()
    
    # If the Inverse is not NULL it returns the saved value
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    # Otherwise the inverse is calculated and set with setinverse()
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    
    # Return the inverse
    I
}


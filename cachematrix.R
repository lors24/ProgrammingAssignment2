## The following pair of functions create a special object that stores a matrix 
## and caches its inverse. 

## makeCacheMatrix creates a special matrix object that can cache its inverse.
## It is a list containing a function to set and get the value of the matrix, 
## and set and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    I<-NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinverse <- function(sol) I <<-sol
    getinverse <- function() I
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
     ## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    I
}


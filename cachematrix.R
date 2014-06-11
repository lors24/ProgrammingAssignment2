

## This funcitn creates a special matrix, which is really a list containing a function to
## set and get the value of the matrix, and set and get the value of the inverse.

set the value of the vector
get the value of the vector
set the value of the mean
get the value of the mean

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
## cacheSolve first checks to see if the inverse has already been calculated.
## If this is the case, it gets the inverse from the cache. 
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


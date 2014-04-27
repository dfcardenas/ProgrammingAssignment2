## This functions helps us to calculate the inverse of a matrix generated with another
##function and it saves that inverse asociated with the matrix in a same object
##in order to look for if it has been already calculated

## This fuction generates a special list function with a matrix and its inverse 
##on its values in order to save this items on cache.

##As its argument say it takes a matrix as only argument

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL    ##Setting null on the inverse
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set= set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)        ##Creating the list object
        }

## This function calculates the the inverse of the matrix retrieved 
##in the other function and stores 
##it on cache in order to not calculate it more than once

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()           ##Review on cache data if the inverse exist
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()         ##If there isn't cahe data for the inverse, it calculates the inverse
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

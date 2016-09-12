## Two functions below are used to create a
## special list object that stores a matrix as 
## well as its inverse.

## The first function, makeCacheMatrix creates
## a list object which contain some function to
## get the value of matrix and set or get the 
## value of the inverse matrix. 


makeCacheMatrix <- function(x = matrix()) { 
    i <- NULL
    get <- function() x
    setInvert <- function(solve) i <<- solve
    getInvert <- function() i
    list(get = get ,
         setInvert = setInvert,
         getInvert = getInvert)
} 



## At the time you first make a list object using
## makeCacheMatrix function, its getInvert function
## will return NULL because you have to do
## solve calculation once to cache the value using 
## the function below.

## This cacheSolve function will first check whether
## the inverse matrix has already been calculated. 
## If so, it gets the value from the cache and skips
## the computation. Otherwise, it calculate the value
## and then set the value using setInvert function.


cacheSolve <- function(x, ...){
    i <- x$getInvert()
    if(!is.null(i)){
        message('getting cached data')
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInvert(i)
    i
}



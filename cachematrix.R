## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	#set the value of matrix
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x  
	#
	#set the value of inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	#Check if inverse is calculated
	inv <- x$getinverse()
	#if inverse is calculated, get cached data
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
	#if inverse is not calculated, get the inverse and set it to variable 'inv' and return
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

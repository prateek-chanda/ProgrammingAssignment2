## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that creates an R object that stores a matrix and its inverse.
## It can be used to instantiate an object that returns a set of 4 functions in a list and
## two data objects, x and i
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL 
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) m <<- inverse 
        getinverse <- function() m 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve requires an argument from makeCacheMatrix to retrieve the inverse of the matrix
## from the cache stored in makeCacheMatrix environment. If the inverse was never calculated and
## stored in the cache, this function will calculate the inverse and store it within makeCacheMatrix
## environment
cacheSolve <- function(x, ...) {
            
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached  data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

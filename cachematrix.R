## This R script contains functions to store and inverse a matrix

## makeCacheMatrix returns a special "matrix" object that can cache itself and its inverse 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)        
}

## cacheSolve returns a matrix that is the inverse of 'x', the special "matrix" object defined in makeCacheMatrix
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        try(inv <- solve(data, ...))
        x$setInverse(inv)
        inv
}

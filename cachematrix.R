## Week 3 Assignment for R Programming

## Aim of the assignment is to write functions that can cache the inverse of a matrix to save time- 
## consuming computations.

## Creates a special matrix that can cache its inverse - assuming the matrix is always invertible. 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}



## Computes the inverse of the special matrix returned by the function above. If the inverse has already
## been calculated and the matrix has not changed then the cacheSolve will retrieve the inverse from the 
## cache.
cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
        inv
}

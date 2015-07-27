## Put comments here that give an overall description of what your
## functions do

## This function gets, sets, and stores the inverse of x. It 

# return a list that contains the output of specific functions within it
# so it can be used as follows:
      # x <- makeCacheMatrix(sample_matrix)
      # x$set(sample_matrix) to set the specific matrix being dealt with
      # x$get to get the matrix
      # x$setinverse to set the inverted matrix
      # x$getinverse to get the inverted matrix
      
makeCacheMatrix <- function(x = matrix()) {
inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_x <<- inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Retrieves the cached inverse of matrix x if it's available.
# If it is not, then it computes and caches it.

cacheSolve <- function(x, ...) {
        inv_x <- x$getinverse()
        if(!is.null(inv_x)) {
                message("getting cached inverse")
                return(inv_x)
        } else
        {
        inv_x <- solve(x$get(), ...)
        x$setinverse(inv_x)
        return(inv_x)
        }
}
}

## Caching the Inverse of a Matrix -
## Functions below caches the inverse of a matrix to reduce processing time for potentially
## time-consuming computations when the a matrix is too big and especially if there are multiple 
## computations (e.g. in a loop).

## The first function, makeCacheMatrix creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
			x <<- y
			i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function computes the inverse of the special matrix computed above.
## First it checks if the inverse has been already calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise it calculates the inverse of the data
## and sets the value of the inverse in cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat_data <- x$get()
        i <- solve(mat_data, ...)
        x$setinverse(i)
        i
}

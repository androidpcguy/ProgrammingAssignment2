## The purpose of the following two functions is to cache the computed inverse 
## of some matrix 'x' so that we do not have to repeatedly compute it. This 
## allows us to save computational resources.

## Creates and returns a special matrix object that is able to cache its inverse.
## Provides useful getters and setters.
makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL
	
	setmatrix <- function(y) {
		x <<- y
		invMatrix <<- NULL
	}

	getmatrix <- function() x
	
	setinvmatrix <- function(inverse) invMatrix <<- inverse
	
	getinvmatrix <- function() invMatrix
	
	list(	
		setmatrix	= setmatrix,
		getmatrix	= getmatrix,
		setinvmatrix	= setinvmatrix,
		getinvmatrix	= getinvmatrix
	)
}


## Computes the inverse of a matrix if it is not saved in our special matrix
## and caches it. If we have already cached the respective inverse matrix,
## it just returns the cached value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	invmatrix <- x$getinvmatrix()
	
	if(is.null(invmatrix)) {
		m <- x$getmatrix()
		invmatrix <- solve(m, ...)
		x$setinvmatrix(invmatrix)
		return(invmatrix)
	}
	message("returning cached inverse")
	invmatrix
}

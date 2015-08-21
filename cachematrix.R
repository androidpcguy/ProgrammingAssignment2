## The purpose of the following two function is to cache the computed inverse 
## of some matrix 'x' so that we do not have to repeatedly compute it. This 
## allows us to save computational resources.

## Creates a special matrix object that is able to cache its inverse.
## Also provides useful getters and setters.

makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL
	setmatrix <- function(y) {
		x <<- y
		invMatrix <<- NULL
	}

	getmatrix <- function() x
	setinvmatrix <- function(inverse) invMatrix <<- inverse
	getinvmatrix <- function() invMatrix
	list(	setmatrix = setmatrix,
		getmatrix = getmatrix,
		setinvmatrix = setinvmatrix,
		getinvmatrix = getinvmatrix
	);
}


## Computes the inverse of a matrix if it is not saved in our special matrix
## and caches it. If we have already cached the respective inverse matrix,
## it just returns the cached value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invmatrix <- x$getinvmatrix()
	##print("got inv matrix")
	if(is.null(invmatrix)) {
		##print("invmatrix is null")
		m <- x$getmatrix()
		##print("got orig matrix")
		invmatrix <- solve(m)
		##print("calculated inv matrix")
		x$setinvmatrix(invmatrix)
		##print("setting inv matrix")
		return(invmatrix)
	}
	##print("returning invmatrix b/c not null")
	x$getinvmatrix()
}

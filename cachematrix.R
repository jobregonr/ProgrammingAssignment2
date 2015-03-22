## This function calculates the inverse of a square invertible matrix
## with the solve function using an identity matrix

## The following function creates a list of the functions to set
## and get the original matrix and the functions to set and get
## the inverse matrix using solve

makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is the original matrix to be inversed
        m <- NULL
	set <- function(y) {
                x <<- y
	        m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
	list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function returns the inverse matrix that resides
## already in the cache and that was calculated in a previous
## execution or the inverse matrix calculated during the execution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m)) {
	        message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}

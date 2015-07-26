## This file has 2 functions:
##	- makeCacheMatrix: create an object with a cacheable inverse.
##	- cacheSolve: returns the inverse of a matrix,
##			using a cached version if present


## Take a matrix as the only argument/parameter.
## Return a list object with the inverse initialized to NULL.
makeCacheMatrix <- function(x = matrix()) {
	# Verify the matrix is square.
	# (Not required for the assignment, but is required to compute an inverse.)
	if(nrow(x) != ncol(x)) {
		stop('Not a square matrix.')
	}
	# Initialize the inverse to NULL.
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inverse <<- solve
	getinverse <- function() inverse
	list(
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## Solve (inverse) a matrix, using a cached version if available.
cacheSolve <- function(x, ...) {
	# Verify the correct object type was given.
	# If you cannot find the names, then it isn't the correct type of object.
	if(is.null(names(x))) {
		stop('Not a valid cacheMatrix object.')
	}
	# If the length of the names vector is wrong, don't compare. It's wrong.
	if(length(names(x)) != 4) {
		stop('Not a valid cacheMatrix object.')
	}
	if(! all(names(x) == c('set', 'get', 'setinverse', 'getinverse'))) {
		stop('Not a valid cacheMatrix object.')
	}
	inverse <- x$getinverse()
	# If the inverse is not null, then use that cached version.
	if(!is.null(inverse)) {
		message('Using cached data.')
		return(inverse)
	}
	# If the inverse IS null,
	# then compute the inverse and store it in the object.
	# (I.e. "cache" the solution.)
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
#
# vim: set tabstop=4:

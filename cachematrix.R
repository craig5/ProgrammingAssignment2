## This file has 2 functions:
##	- makeCacheMatrix: create an object with a cacheable inverse.
##	- cacheSolve: returns the inverse of a matrix,
##			using a cached version if present


## Take a matrix as the only argument/parameter.
## Return a list object with the inverse initialized to NULL.
makeCacheMatrix <- function(x = matrix()) {
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

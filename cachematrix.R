# makeCacheMatrix gets an invertible matrix as the only argument and
# returns a list of four functions to set/get its value and set/get its
# inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL	# Value of inverse matrix initially set to null object
	
	# The set function takes the matrix as an input and resets the inverse
	# value to NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() x	# The get function returns the matrix value

	# The setinv function calculates the inverse
	setinv <- function(solve) inv <<- solve
	
	# The getinv function returns the inverse matrix value
	getinv <- function() inv

	# The function list groups the four functions in one named list
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve takes the list of four functions defined by makeCacheMatrix
# as the argument and either calculates the inverse matrix (if no previously
# calculated value is available) or returns an existing value under "inv" without
# performing further inverse calculations.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()	# Gets current value of inv

	# The if test checks if the inverse matrix has already been calculated
	# and if so returns the inv value.
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	# If no inverse matrix has been calculated it first gets the matrix
	data <- x$get()

	# Then calculates the its inverse
	inv <- solve(data, ...)
	
	# Sets the new value
	x$setinv(inv)

	inv # And returns it
}
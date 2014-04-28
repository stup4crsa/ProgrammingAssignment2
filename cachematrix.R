## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix returns a list of 4 functions, to call this do:
# mx <- makeCacheMatrix()
# The 4 function can be accessed as follows:
# mx$set(matrix(sample(1:25), 5, 5))
# mx$get() - 
# mx$setinv(val) - fills in the inverse storage area with val
# mx$getinv() - returns the value strored in the inverse storage area

makeCacheMatrix <- function(mat = matrix()) {
	# on first creation, mark inverse NULL indicating needs to be computed
	inv <- NULL
	set <- function(y) {
		# put stuff in our special environment area:
		# mat is our chunk of data (the matrix)
		mat <<- y
		# any time the matrix is set, mark the inverse as needing to be
		# computed.  Should be any time the matrix CHANGES, not just set.
		inv <<- NULL
		message('set inv to NULL on change of matrix')
	}

	# just return the data
	get <- function () mat

	# pass in a value to be used as the inverse from now on, or at least
	# until the data is reset
	setinv <- function(value) inv <<- value;

	# get the stored value for the inverse.  Despite the fact that the name
	# might imply that the inv is calcuated, it is not calculated here.
	# you must do that externally.
	getinv <- function() inv

	# return value is list of the above 4 functions:
	list(set = set, get = get, setinv = setinv, getinv = getinv)	
}


## Write a short comment describing this function
# first time this is called, it needs to calucate the inverse,
# next times (unless the matrix was re-set) it returns the calcuated
# value
cacheSolve <- function(x) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if (is.null(i)) {
		message('calculating inverse')
		i <- solve(x$get())
		x$setinv(i)
		return(i)
	} else {
		message('using cached inverse')
		return (i)
	}
}

# to use / test:
# m1 <- matrix(sample(1:25),5,5)
# m2 <- matrix(sample(1:25),5,5)
# mx <- makeCacheMatrix()
# mx$set(m1)
# cacheSolve(mx)
# cacheSolve(mx)
# mx$set(m2)
# cacheSolve(mx)
# cacheSolve(mx)

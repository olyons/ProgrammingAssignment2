## makeCachematrix creates a "matrix" with functions to get and set 
## the value of the matrix, as well as get and set the value of the inverse


## Write a short comment describing this function

## makeCacheMatrix takes a matrix (as the variable x)

makeCacheMatrix <- function(x = matrix()) {
        ## The inverse (i) is set to NULL / initialize the inverse variable
        i <- NULL

	## Set the matrix and inverse
	set <- function(y) {
	        x <<- y
		i <<- NULL
	}
	
	## Get method -- get the matrix (x)
	get <- function() x

	## Method to set the inverse matrix
	setinverse <- function(inverse) i <<- inverse
	
	## Method to get the inverse matrix
	getinverse <- function() i
	
	## List of methods
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## cacheSolve will compute the inverse of the matrix using makeCacheMatrix

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()

	## Check to see if the inverse has been computed
	if(!is.null(i)) {
	        message("getting cached data")
		return(i)
	}

	## If the the inverse isn't computed
	## Get the matrix
	data <- x$get()

	## Compute the inverse matrix
	i <- solve(data, ...)

	## Set the inverse matrix
	x$setinverse(i)

	## Return the inverse matrix
	i
}

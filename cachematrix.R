## This function does the following:
## -initializes inv to NULL
## -creates the function set()
# # -creates the function get
# # -creates the function setinv
# # -creates the function getinv
# # -puts those functions in a list, to be used by the function cacheSolve
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL #at initiation, the inverse is NULL
	set <- function(y) {
			x <<- y
			inv <<- NULL
	}
	get <- function() x #just returns the value of the matrix
	setinv <- function(solve) inv <<- solve #gives the solve function to be used
	getinv <- function() inv #returns the inverse
	list(set = set, get = get, #put everything in a list
		 setinv = setinv,
		 getinv = getinv)
}
#this function does the following:
cacheSolve <- function(x, ...) {
	inv <- x$getinv() #first retrieves the inverse, whatever that might be
	if(is.null(inv)) { #check if the matrix is NULL.  
		#Only if the inverse has not been calculated, the cost of the computation is paid
		inv <- solve(x$get(), ...)
		x$setinv(inv) #the result is set in the object for future reference
	}
	else { #if the inverse has been calculated, nothing is done, except printing a message
		message("getting cached data")
	}
	inv #returning the inverse
}

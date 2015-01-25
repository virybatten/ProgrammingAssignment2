#It creates matrix object that 
#can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(xx) {
		x <<- xx;
		inv <<- NULL;
	}
	get <- function() return(x);
	setinv <- function(inv) inv <<- inv;
	getinv <- function() return(inv);
	return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


#It computes the inverse of the 
#matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		return(inv)
	}
	data <- x$get()
	invserse <- solve(data, ...)
	x$setinv(inv)
	return(inv)
}
